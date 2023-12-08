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

  // 8(%rbp): return address (not used).
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
  body.Append(
      new assem::OperInstr("", nullptr, reg_manager->ReturnSink(), nullptr));
}

assem::Proc *X64Frame::procEntryExit3(assem::InstrList *body) const {

  std::stringstream prologue_ss{}, epilogue_ss{};
  prologue_ss << temp::LabelFactory::LabelString(this->name_) << ":";

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
    body->Prepend(new assem::OperInstr(ss.str(), new temp::TempList{rsp},
                                       new temp::TempList{rsp}, nullptr));
  } while (0);

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
    body->Prepend(new assem::MoveInstr("movq `s0, `d0", new temp::TempList(rbp),
                                       new temp::TempList(rsp)));

    body->Prepend(new assem::OperInstr("movq `s0, (`s1)", nullptr,
                                       new temp::TempList{rbp, rsp}, nullptr));

    std::stringstream ss{};
    ss << "subq $" << KX64WordSize << ", `d0";
    body->Prepend(new assem::OperInstr(ss.str(), new temp::TempList{rsp},
                                       new temp::TempList{rsp}, nullptr));
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
    body->Append(new assem::OperInstr("movq (`s0), `s1", nullptr,
                                      new temp::TempList{rsp, rbp}, nullptr));
    std::stringstream ss{};
    ss << "addq $" << KX64WordSize << ", `d0";
    body->Append(new assem::OperInstr(ss.str(), new temp::TempList{rsp},
                                      new temp::TempList{rsp}, nullptr));
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

} // namespace frame
