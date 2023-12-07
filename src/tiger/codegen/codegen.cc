#include "tiger/codegen/codegen.h"

#include <cassert>
#include <sstream>

extern frame::RegManager *reg_manager;

namespace {

constexpr int maxlen = 1024;

} // namespace

namespace cg {

void CodeGen::Codegen() {
  auto stm_list = this->traces_->GetStmList();
  auto instr_list = new assem::InstrList();
  this->assem_instr_ = std::make_unique<AssemInstr>(instr_list);
  for (auto stm : stm_list->GetList()) {
    stm->Munch(*instr_list, this->fs_);
  }
  this->frame_->procEntryExit2(*instr_list);
}

void AssemInstr::Print(FILE *out, temp::Map *map) const {
  for (auto instr : instr_list_->GetList())
    instr->Print(out, map);
  fprintf(out, "\n");
}
} // namespace cg

namespace tree {

using namespace std::string_literals;
using namespace std::string_view_literals;

bool is_const_exp(tree::Exp *exp) noexcept {
  return typeid(*exp) == typeid(tree::ConstExp);
}

bool is_frame_pointer(tree::Exp *exp) noexcept {
  if (typeid(*exp) != typeid(tree::TempExp)) {
    return false;
  }
  auto temp_exp = static_cast<tree::TempExp *>(exp);
  return temp_exp->temp_ == reg_manager->FramePointer();
}

/**
 * Assumption: a prior check of @c is_const_exp(exp) returns true.
 */
int deConstExp(tree::Exp *exp) {
  assert(is_const_exp(exp));
  return static_cast<tree::ConstExp *>(exp)->consti_;
}

// We don't need emit function described on the textbook (P212). Just use
// assem::InstrList::Append.

void SeqStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  this->left_->Munch(instr_list, fs);
  this->right_->Munch(instr_list, fs);
}

void LabelStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  instr_list.Append(new assem::LabelInstr(
      temp::LabelFactory::LabelString(this->label_), this->label_));
}

void JumpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto jump_targets = new std::vector<temp::Label *>(*this->jumps_);
  instr_list.Append(new assem::OperInstr("jmp `j0"s, nullptr, nullptr,
                                         new assem::Targets(jump_targets)));
}

void CjumpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  static std::unordered_map<tree::RelOp, const char *> op_jmp_mapper = {
      {EQ_OP, "je"},   {NE_OP, "jne"}, {LT_OP, "jl"},   {GT_OP, "jg"},
      {LE_OP, "jle"},  {GE_OP, "jge"}, {ULT_OP, "jge"}, {ULE_OP, "jg"},
      {UGT_OP, "jle"}, {UGE_OP, "jl"},
  };

  auto left_r = this->left_->Munch(instr_list, fs);
  auto right_r = this->right_->Munch(instr_list, fs);
  instr_list.Append(new assem::OperInstr(
      "cmpq `s0, `s1"s, nullptr, new temp::TempList{left_r, right_r}, nullptr));
  std::stringstream ss{};
  ss << op_jmp_mapper.find(this->op_)->second << " `j0";
  auto jump_targets = new std::vector<temp::Label *>{this->true_label_};
  instr_list.Append(new assem::OperInstr(ss.str(), nullptr, nullptr,
                                         new assem::Targets(jump_targets)));
}

void MoveStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto src_r = this->src_->Munch(instr_list, fs);

  if (typeid(*this->dst_) == typeid(tree::MemExp)) {
    auto mem_exp = static_cast<tree::MemExp>(this->dst_);
    assert(typeid(*mem_exp.exp_) == typeid(tree::BinopExp));
    auto mem_addr_exp = static_cast<tree::BinopExp *>(mem_exp.exp_);
    assert(mem_addr_exp->op_ == tree::BinOp::PLUS_OP);
    auto src_r = this->src_->Munch(instr_list, fs);

    if (!is_const_exp(mem_addr_exp->left_) && !is_const_exp(mem_addr_exp)) {
      auto addr_r = mem_addr_exp->Munch(instr_list, fs);
      auto instr =
          new assem::OperInstr("movq `s0, (`s1)"s, nullptr,
                               new temp::TempList({src_r, addr_r}), nullptr);
      instr_list.Append(instr);
    } else {
      int offset = 0; // in byte
      temp::Temp *base_addr_r = nullptr;
      if (is_const_exp(mem_addr_exp->left_)) {
        offset = static_cast<tree::ConstExp *>(mem_addr_exp->left_)->consti_;
        base_addr_r = mem_addr_exp->right_->Munch(instr_list, fs);
      } else {
        offset = static_cast<tree::ConstExp *>(mem_addr_exp->right_)->consti_;
        base_addr_r = mem_addr_exp->left_->Munch(instr_list, fs);
      }
      std::stringstream ss{};
      ss << "movq `s0, " << offset << "(`s1)";
      auto instr = new assem::OperInstr(
          ss.str(), nullptr, new temp::TempList({src_r, base_addr_r}), nullptr);
      instr_list.Append(instr);
    }

  } else if (typeid(*this->dst_) == typeid(tree::TempExp)) {
    auto dst_exp = static_cast<tree::TempExp *>(this->dst_);
    auto dst_r = dst_exp->temp_;
    instr_list.Append(new assem::MoveInstr("movq `s0, `d0"s,
                                           new temp::TempList({dst_r}),
                                           new temp::TempList({src_r})));
  } else {
    assert(0);
  }
}

void ExpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  this->exp_->Munch(instr_list, fs);
}

temp::Temp *BinopExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  // Prologue
  // It's meaningless, should never occur.
  assert(!(is_const_exp(this->left_) && is_const_exp(this->right_)));
  // Canon should handle it, but I don't know if it has.
  if (!is_const_exp(this->left_)) {
    auto tmp = this->left_;
    this->right_ = this->left_;
    this->left_ = this->right_;
  }

  // Now if there is a const, if must occur on the left.

  // Begin to handle BinopExp
  // Left side can be immediate or register.
  struct {
    union {
      int val;
      temp::Temp *reg;
    } u;
    bool is_const;
  } lhs{{}, false};

  // Handle the left side, where constant may occur.
  if (is_const_exp(this->left_)) {
    lhs.u.val = deConstExp(this->left_);
    lhs.is_const = true;
  } else {
    auto left_r = this->left_->Munch(instr_list, fs);
    lhs.u.reg = left_r;
    lhs.is_const = false;
  }

  // Right operand, also the returned register.
  auto right_r = this->right_->Munch(instr_list, fs);

  using tree::BinOp;
  // We only implement 4 kinds of operations: add, minus, divide and multiply.
  std::string i_oper{}; // Instruction operator
  if (this->op_ == PLUS_OP) {
    i_oper = "addq";
  } else if (this->op_ == MINUS_OP) {
    i_oper = "subq";
  } else if (this->op_ == MUL_OP) {
    // The form: imulp s. Effect: put RAX * s to RDX:RAX.
    auto rax = reg_manager->GetRegister(static_cast<int>(frame::Register::RAX));
    auto rdx = reg_manager->GetRegister(static_cast<int>(frame::Register::RDX));
    // Move the lhs to RAX
    if (lhs.is_const) {
      std::stringstream ss{};
      ss << "movq $" << lhs.u.val << ", `d0";
      instr_list.Append(
          new assem::MoveInstr(ss.str(), new temp::TempList{rax}, nullptr));
    } else {
      instr_list.Append(new assem::MoveInstr("movq `s0, `d0",
                                             new temp::TempList{rax},
                                             new temp::TempList{lhs.u.reg}));
    }
    instr_list.Append(
        new assem::OperInstr("imulq `s0", new temp::TempList{rax, rdx},
                             new temp::TempList{right_r, rax}, nullptr));
    return rax;
  } else if (this->op_ == DIV_OP) {
    // TODO divide
    assert(0);
  } else {
    assert(0);
  }
  // Normal case
  assert(!i_oper.empty());
  auto src_regs = new temp::TempList{};
  auto dst_regs = new temp::TempList{};
  std::string lhs_name{};
  if (lhs.is_const) {
    lhs_name = "$" + std::to_string(lhs.u.val);
  } else {
    lhs_name = "`s0";
    src_regs->Append(lhs.u.reg);
  }
  std::stringstream ss{};
  ss << i_oper << " " << lhs_name << ", `d0";
  src_regs->Append(right_r);
  dst_regs->Append(right_r);
  instr_list.Append(
      new assem::OperInstr(ss.str(), dst_regs, src_regs, nullptr));
  return right_r;
}

temp::Temp *MemExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  // Here we only handle the fetch of memory,
  // as store must be munched by MoveStm#Munch.
  auto r = temp::TempFactory::NewTemp();
  auto dst_r = new temp::TempList({r});
  // A fact: For Tiger, all MemExp we translated is in the form of
  // MEM(+(exp, exp))
  assert(typeid(*this->exp_) == typeid(tree::BinopExp));
  auto op_exp = static_cast<tree::BinopExp *>(this->exp_);
  assert(op_exp->op_ == tree::BinOp::PLUS_OP);

  assert(!(is_const_exp(op_exp->left_) && is_const_exp(op_exp->right_)));

  if (!is_const_exp(op_exp->left_) && !is_const_exp(op_exp->right_)) {
    // TODO enable (t11, t12, 4) format
    auto mem_addr_r = this->exp_->Munch(instr_list, fs);
    auto instr = new assem::OperInstr(
        "movq (`s0), `d0"s, dst_r, new temp::TempList({mem_addr_r}), nullptr);
    instr_list.Append(instr);
  } else {
    int offset = 0; // in byte
    temp::Temp *base_addr_r = nullptr;
    if (is_const_exp(op_exp->left_)) {
      offset = static_cast<tree::ConstExp *>(op_exp->left_)->consti_;
      base_addr_r = op_exp->right_->Munch(instr_list, fs);
    } else {
      offset = static_cast<tree::ConstExp *>(op_exp->right_)->consti_;
      base_addr_r = op_exp->left_->Munch(instr_list, fs);
    }
    std::stringstream ss{};
    ss << "movq " << offset << "(`s0), `d0";
    auto instr = new assem::OperInstr(
        ss.str(), dst_r, new temp::TempList({base_addr_r}), nullptr);
    instr_list.Append(instr);
  }

  return r;
}

temp::Temp *TempExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  return this->temp_;
}

temp::Temp *EseqExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  this->stm_->Munch(instr_list, fs);
  return this->exp_->Munch(instr_list, fs);
}

temp::Temp *NameExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  assert(0);
}

temp::Temp *ConstExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  // TODO ConstExp should never be munched!
  // assert(0);
  auto r = temp::TempFactory::NewTemp();
  std::stringstream ss{};
  ss << "movq $" << this->consti_ << ", `d0";
  instr_list.Append(new assem::OperInstr(ss.str(), new temp::TempList({r}),
                                         nullptr, nullptr));
  return r;
}

temp::Temp *CallExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  assert(typeid(*this->fun_) == typeid(tree::NameExp));

  auto r = reg_manager->ReturnValue();
  auto args = this->args_->MunchArgs(instr_list, fs);

  auto instr_str =
      "callq " + temp::LabelFactory::LabelString(
                     static_cast<tree::NameExp *>(this->fun_)->name_);
  auto instr =
      new assem::OperInstr(instr_str, new temp::TempList({r}), args, nullptr);

  instr_list.Append(instr);
  return r;
}

temp::TempList *ExpList::MunchArgs(assem::InstrList &instr_list,
                                   std::string_view fs) {
  auto r_list = new temp::TempList();
  temp::TempList *convention_args = reg_manager->ArgRegs();
  auto arg_cnt = this->exp_list_.size();
  auto reg_cnt = convention_args->GetList().size();

  // Munch to select the instructions.
  auto arg_exp_it = this->exp_list_.begin();
  for (int i = 0; i < std::min(arg_cnt, reg_cnt); ++i, next(arg_exp_it)) {
    // Update returned register list
    r_list->Append(convention_args->NthTemp(i));

    tree::Exp *arg_exp = *arg_exp_it;
    auto dst_regs = new temp::TempList({convention_args->NthTemp(i)});

    if (is_const_exp(arg_exp)) {
      std::stringstream ss{};
      ss << "movq $" << deConstExp(arg_exp) << ", `d0";
      auto instr = new assem::MoveInstr(ss.str(), dst_regs, nullptr);
      instr_list.Append(instr);
    } else {
      // Get the argument from another register.
      auto src_regs = new temp::TempList(arg_exp->Munch(instr_list, fs));
      auto instr =
          new assem::OperInstr("movq `s0, `d0"s, dst_regs, src_regs, nullptr);
      instr_list.Append(instr);
    }
  }

  // Some arguments need to be pushed onto the stack.
  std::string instr_dec_rsp =
      "subq $" + std::to_string(frame::KX64WordSize) + ", `d0";

  for (int offset_word = 0; arg_exp_it != this->exp_list_.end();
       offset_word++, next(arg_exp_it)) {

    auto rsp = reg_manager->StackPointer();
    tree::Exp *arg_exp = *arg_exp_it;
    auto src_regs = new temp::TempList({rsp});

    // e.g. 8(%rsp)
    std::string dst_name =
        std::to_string(offset_word * frame::KX64WordSize) + "(`s0)";

    std::stringstream ss{};
    if (is_const_exp(arg_exp)) {
      // Literal argument
      ss << "movq $" << deConstExp(arg_exp) << ", " << dst_name;
    } else {
      // Get the argument from another register.
      src_regs->Append(arg_exp->Munch(instr_list, fs));
      ss << "movq `s1, " << dst_name;
    }

    instr_list.Append(new assem::OperInstr(instr_dec_rsp,
                                           new temp::TempList{rsp},
                                           new temp::TempList{rsp}, nullptr));
    instr_list.Append(
        new assem::OperInstr(ss.str(), nullptr, src_regs, nullptr));
  }

  return r_list;
}

} // namespace tree
