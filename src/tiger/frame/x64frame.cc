#include "tiger/frame/x64frame.h"
#include <sstream>

extern frame::RegManager *reg_manager;

namespace frame {

X64RegManager::X64RegManager() {
  for (int i = 0; i < static_cast<int>(Register::COUNT); ++i) {
    auto temp = temp::TempFactory::NewTemp();
    this->regs_.push_back(temp);
    auto name = new std::string{KRegisterNames.at(i)};
    this->temp_map_->Enter(temp, name);
  }
}

class InFrameAccess : public Access {
public:
  /// @note The offset is in byte. No need to multiply the work size.
  int offset;

  explicit InFrameAccess(int offset) : offset(offset) {}

  tree::Exp *toExp(tree::Exp *framePtr) const override {
    // Simple variable. The access depends on the frame pointer.
    return new tree::MemExp(new tree::BinopExp(tree::BinOp::PLUS_OP, framePtr,
                                               new tree::ConstExp(offset)));
  }
};

class InRegAccess : public Access {
public:
  temp::Temp *reg;

  explicit InRegAccess(temp::Temp *reg) : reg(reg) {}
  tree::Exp *toExp(tree::Exp *framePtr) const override {
    // A variable in the register.
    // Independent with the frame pointer, so unused.
    return new tree::TempExp(reg);
  }
};

frame::Frame *newFrame(temp::Label *f, const std::list<bool> &formals) {
  auto x64_frame = new X64Frame(f, formals);
  return x64_frame;
}

X64Frame::X64Frame(temp::Label *label, const std::list<bool> &formals)
    : name_(label), neg_local_offset_(0), view_shift_stm_(nullptr) {

  auto arg_regs = reg_manager->ArgRegs();
  auto arg_reg_cnt = arg_regs->GetList().size();
  std::list<tree::Stm *> stm_list{};

  auto escape_it = begin(formals);
  for (int i = 0; i < std::min(arg_reg_cnt, formals.size()); ++i, ++escape_it) {

    assert(escape_it != end(formals));

    auto formal_access = this->allocateLocal(true); // X86 stack
    stm_list.push_back(new tree::MoveStm(
        formal_access->toExp(new tree::TempExp(reg_manager->FramePointer())),
        new tree::TempExp(arg_regs->NthTemp(i))));

    this->formals_.push_back(formal_access);
  }

  for (int offset = 16; escape_it != end(formals);
       ++escape_it, offset += KX64WordSize) {
    auto formal_access = new InFrameAccess(offset);
    this->formals_.push_back(formal_access);
  }

  // 8(%rbp): return address (not used), set by `call` instruction.
  // (%rbp): previous %rbp value. Let #procEntryExit3 handles it.

  // Combine all statements.
  if (stm_list.empty()) {
    return;
  }
  this->view_shift_stm_ = stm_list.back();
  stm_list.pop_back();
  while (!stm_list.empty()) {
    auto stm = stm_list.back();
    stm_list.pop_back();
    this->view_shift_stm_ = new tree::SeqStm(stm, this->view_shift_stm_);
  }
}

frame::Access *X64Frame::allocateLocal(bool escape) {
  if (!escape) {
    auto temp = temp::TempFactory::NewTemp();
    return new frame::InRegAccess(temp);
  }
  this->neg_local_offset_ += KX64WordSize;
  auto access = new frame::InFrameAccess(-this->neg_local_offset_);
  return access;
}

tree::Stm *X64Frame::procEntryExit1(tree::Stm *stm) const {
  if (this->view_shift_stm_ == nullptr) {
    return stm;
  }
  return new tree::SeqStm(this->view_shift_stm_, stm);
}

void X64Frame::procEntryExit2(assem::InstrList &body) const {
  // Remove redundant jump statements.
  // TODO count zero-ref label?
  auto instr_list = body.GetList();
  std::list<assem::Instr *> to_remove{};
  for (auto it = begin(instr_list); it != end(instr_list); ++it) {
    if (next(it) == end(instr_list)) {
      continue;
      // if (typeid(*it) == typeid(assem::LabelInstr)) {
      //   to_remove.push_back(*it);
      // }
    }
    auto cur_instr = *it;
    if (typeid(*cur_instr) != typeid(assem::OperInstr)) {
      continue;
    }
    auto op_instr = static_cast<assem::OperInstr *>(cur_instr);
    if (op_instr->jumps_ == nullptr) {
      continue;
    }
    if (op_instr->assem_.compare(0, 3, "jmp") != 0) {
      continue;
    }
    assert(op_instr->jumps_);
    assert(op_instr->jumps_->labels_->size() == 1);
    auto target = op_instr->jumps_->labels_->front();
    auto next_instr = *next(it);
    if (typeid(*next_instr) != typeid(assem::LabelInstr)) {
      continue;
    }
    auto next_label = static_cast<assem::LabelInstr *>(next_instr)->label_;
    if (target == next_label) {
      to_remove.push_back(cur_instr);
    }
  }
  printf("--------------------------------\n[ProcEntryExit2] eliminate "
         "following instructions\n");
  for (auto instr : to_remove) {
    instr->Print(stdout, nullptr);
    body.Remove(instr);
  }
  printf("--------------------------------\n");

  body.Append(
      new assem::OperInstr("", nullptr, reg_manager->ReturnSink(), nullptr));
}

assem::Proc *X64Frame::procEntryExit3(assem::InstrList *body) const {

  std::stringstream prologue_ss{}, epilogue_ss{};
  prologue_ss << temp::LabelFactory::LabelString(this->name_) << ":";

  {
    // Prepend instructions. Collect them in a list, and reversely prepend.
    std::list<assem::Instr *> to_prepend{};
    {
      /**
       * Prepend something equivalent to:
       * pushq %rbp  ; => subq $8, %rsp
       *             ;    movq %rbp, (%rsp)
       * movq %rsp, %rbp
       *
       */
      auto rsp = reg_manager->GetRegister(static_cast<int>(Register::RSP));
      auto rbp = reg_manager->GetRegister(static_cast<int>(Register::RBP));

      auto push_instr_list = makePushInstr(rbp);
      to_prepend.insert(end(to_prepend), begin(push_instr_list),
                        end(push_instr_list));
      to_prepend.push_back(new assem::MoveInstr(
          "movq `s0, `d0", new temp::TempList(rbp), new temp::TempList(rsp)));
    }

    do {
      /**
       * Adjust stack pointer.
       */
      if (this->neg_local_offset_ == 0) {
        break;
      }
      auto rsp = reg_manager->StackPointer();
      std::stringstream ss{};
      ss << "subq $" << this->neg_local_offset_ << ", `d0";
      to_prepend.push_back(new assem::OperInstr(
          ss.str(), new temp::TempList{rsp}, new temp::TempList{rsp}, nullptr));
    } while (0);

    for (auto it = rbegin(to_prepend); it != rend(to_prepend); ++it) {
      body->Prepend(*it);
    }
  }

  {
    /**
     * Restore the stack pointer.
     * Append something equivalent to:
     * leave ; => movq %rbp, %rsp
     *       ;    popq %rbp        ; => movq (%rsp), %rbp
     *       ;                     ; => addq $8, %rsp
     */
    auto rsp = reg_manager->StackPointer();
    auto rbp = reg_manager->FramePointer();

    body->Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(rsp),
                                      new temp::TempList(rbp)));
    body->Concat(makePopInstr(rbp));
  }

  {
    // Return
    auto rax = reg_manager->GetRegister(static_cast<int>(Register::RAX));
    body->Append(new assem::OperInstr("retq", nullptr, new temp::TempList{rax},
                                      nullptr));
  }

  endl(prologue_ss);
  endl(epilogue_ss);
  return new assem::Proc(prologue_ss.str(), body, epilogue_ss.str());
}

std::list<assem::Instr *> makePushInstr(Immediate con) {
  auto rsp = reg_manager->GetRegister(static_cast<int>(Register::RSP));
  std::list<assem::Instr *> res{};
  std::stringstream ss{};
  ss << "subq $" << KX64WordSize << ", `d0";
  res.push_back(new assem::OperInstr(ss.str(), new temp::TempList{rsp},
                                     new temp::TempList{rsp}, nullptr));
  ss.str("");
  ss << "movq $" << con << ", (`s0)";
  res.push_back(new assem::OperInstr(ss.str(), nullptr, new temp::TempList{rsp},
                                     nullptr));
  return res;
}

std::list<assem::Instr *> makePushInstr(temp::Temp *operand) {
  auto rsp = reg_manager->GetRegister(static_cast<int>(Register::RSP));
  std::list<assem::Instr *> res{};
  std::stringstream ss{};
  ss << "subq $" << KX64WordSize << ", `d0";
  res.push_back(new assem::OperInstr(ss.str(), new temp::TempList{rsp},
                                     new temp::TempList{rsp}, nullptr));
  res.push_back(new assem::OperInstr(
      "movq `s0, (`s1)", nullptr, new temp::TempList{operand, rsp}, nullptr));
  return res;
}

std::list<assem::Instr *> makePopInstr(temp::Temp *operand) {
  auto rsp = reg_manager->GetRegister(static_cast<int>(Register::RSP));
  std::list<assem::Instr *> res{};
  res.push_back(new assem::OperInstr(
      "movq (`s0), `s1", nullptr, new temp::TempList{rsp, operand}, nullptr));

  std::stringstream ss{};
  ss << "addq $" << KX64WordSize << ", `d0";
  res.push_back(new assem::OperInstr(ss.str(), new temp::TempList{rsp},
                                     new temp::TempList{rsp}, nullptr));
  return res;
}
} // namespace frame
