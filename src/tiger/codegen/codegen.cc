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

bool is_temp_exp(tree::Exp *exp) noexcept {
  return typeid(*exp) == typeid(tree::TempExp);
}

bool is_mem_exp(tree::Exp *exp) noexcept {
  return typeid(*exp) == typeid(tree::MemExp);
}

/**
 * Assumption: a prior check of @c is_const_exp(exp) returns true.
 */
int deConstExp(tree::Exp *exp) {
  assert(is_const_exp(exp));
  return static_cast<tree::ConstExp *>(exp)->consti_;
}

temp::Temp *deTempExp(tree::Exp *exp) {
  assert(is_temp_exp(exp));
  return static_cast<tree::TempExp *>(exp)->temp_;
}

struct Exp::MaybeConst {
  bool is_const;
  union {
    frame::Immediate val;
    temp::Temp *reg;
  } u;

  MaybeConst(int v) : is_const(true) { u.val = v; }

  MaybeConst(temp::Temp *reg) : is_const(false) { u.reg = reg; }
};

auto Exp::MunchIfConst(assem::InstrList &instr_list, std::string_view fs,
                       bool is_volatile) -> MaybeConst {
  // A fallback resolution for other Exps (except for ConstExp and TempExp).
  return MaybeConst(this->Munch(instr_list, fs));
}

auto Exp::MunchIfConst(assem::InstrList &instr_list, std::string_view fs)
    -> MaybeConst {
  return this->MunchIfConst(instr_list, fs, true);
}

auto ConstExp::MunchIfConst(assem::InstrList &instr_list, std::string_view fs,
                            bool is_volatile) -> MaybeConst {
  return MaybeConst(deConstExp(this));
}

auto TempExp::MunchIfConst(assem::InstrList &instr_list, std::string_view fs,
                           bool is_volatile) -> MaybeConst {
  if (!is_volatile) {
    // Directly pass the register, because the parent node promises the context
    // is non-volatile, i.e. the register will never be overwritten.
    return MaybeConst(deTempExp(this));
  } else {
    // Pass the munch result, since it's always safe!
    return MaybeConst(this->Munch(instr_list, fs));
  }
}

/**
 * @brief Return type of MemExp#MunchInMemory.
 *
 */
struct MemExp::InMemoryOperand {
  /// The operand string.
  std::string operand;
  /// All registers that need to be specified in the src_ to be filled in
  /// later.
  /// @note The indexes correspond to #operand, always beginning with 0.
  /// So you need to carefully treat the returned information, for example, the
  /// regs in src_regs should always be put on the front.
  temp::TempList *src_regs;
};

/**
 * @brief Munch a memory representation.
 *
 * The forms are:
 *
 *      Simplest form
 *          form 1
 *           MEM
 *            |
 *           PLUS
 *           / \
 *             CONST
 *
 *      form 1       form 2           form 3             form 4
 *       MEM          MEM              MEM                MEM
 *        |            |                |                  |
 *       PLUS         PLUS             PLUS               PLUS
 *       / \          / \            /      \           /      \
 *         MUL          MUL        MEM       MUL      MEM       MUL      ...
 *         / \          / \         |        / \       |        / \
 *          CONST   CONST CONST   PULS        CONST  PULS  CONST  CONST
 *                                 / \                / \
 *                                  CONST              CONST
 *
 * e.g.  arr[i]       arr[2]
 *
 * @note We only handle the first three forms. The others can be munched out
 * naturally.
 *
 * @par Why is the memory form Imm(r1, r2, s) never used?
 * Because all arrays in Tiger are allocated in heap. This form of memory is
 * convenient when dealing with C-type in-frame arrays.
 *
 * @sa tr::makeSubscript
 *
 * @tparam _ReturnTy InMemoryOperand
 * @param instr_list
 * @param fs
 * @return @sa InMemoryOperand
 */
template <typename _ReturnTy>
auto MemExp::MunchInMemory(assem::InstrList &instr_list, std::string_view fs)
    -> _ReturnTy {

  /**
   * @brief Represent a simple form of in-memory operand.
   *
   */
  struct SimpleInMemory {
    temp::Temp *reg;
    frame::Immediate val;
  };

  // Handler to munch the simplest type of MemExp.
  constexpr auto simpleMunch = [](MemExp *mem_exp, assem::InstrList &instr_list,
                                  std::string_view fs) -> SimpleInMemory {
    /**
     * Only handles the simplest form:
     *
     *       MEM
     *        |
     *      PLUS
     *       / \
     *         CONST
     */
    assert(typeid(*mem_exp->exp_) == typeid(tree::BinopExp));
    auto op_exp = static_cast<tree::BinopExp *>(mem_exp->exp_);
    assert(op_exp->op_ == tree::BinOp::PLUS_OP);

    auto reg_res = op_exp->left_->MunchIfConst(instr_list, fs, false);
    auto const_res = op_exp->right_->MunchIfConst(instr_list, fs, false);
    assert(!reg_res.is_const && const_res.is_const);

    return {reg_res.u.reg, const_res.u.val};
  };

  if (typeid(*this->exp_) == typeid(tree::BinopExp)) {
    auto op_exp = static_cast<tree::BinopExp *>(this->exp_);
    assert(op_exp->op_ == tree::BinOp::PLUS_OP);

    if (typeid(*op_exp->right_) == typeid(tree::BinopExp)) {
      // Subscript.
      auto right_exp = static_cast<tree::BinopExp *>(op_exp->right_);
      assert(right_exp->op_ == tree::BinOp::MUL_OP);
      assert(is_const_exp(right_exp->right_));

      auto s0_res = op_exp->left_->MunchIfConst(instr_list, fs, false);
      assert(!s0_res.is_const);

      auto s1_res = right_exp->left_->MunchIfConst(instr_list, fs, false);
      // Expected to equal to word size (8).
      auto per_bias = right_exp->right_->MunchIfConst(instr_list, fs).u.val;
      assert(per_bias == frame::KX64WordSize);

      if (s1_res.is_const) {
        // form 2
        std::stringstream ss{};
        if (auto offset = s1_res.u.val * per_bias; offset != 0) {
          ss << offset;
        }
        ss << "(`s0)";
        return {ss.str(), new temp::TempList{s0_res.u.reg}};
      } else {
        // form 1
        std::stringstream ss{};
        ss << "(`s0,`s1," << per_bias << ")";
        return {ss.str(), new temp::TempList{s0_res.u.reg, s1_res.u.reg}};
      }
    }

    {
      /**
       *       MEM
       *        |
       *      PLUS
       *       / \
       *         CONST
       */
      SimpleInMemory simple_mem_res = simpleMunch(this, instr_list, fs);

      std::stringstream ss{};
      if (simple_mem_res.val != 0) {
        ss << simple_mem_res.val;
      }
      ss << "(`s0)";
      return {ss.str(), new temp::TempList{simple_mem_res.reg}};
    }
  }
  // In Tiger, no other cases.
  assert(0);

  auto r = this->exp_->Munch(instr_list, fs);
  return {"(`s0)", new temp::TempList{r}};
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
  do {
    /**
     * cmp instruction has the following forms:
     *      cmp   a  ,   b
     *      cmp <reg>, <reg>
     *      cmp <mem>, <reg>
     *      cmp <reg>, <mem>
     *      cmp <con>, <reg>
     * Since the result is base on b-a, we need to fill this->left_ on the right
     * and this->right_ on the left.
     * But the precedence counts. That is, this->left_ still need to be
     * evaluated before this->right_.
     */
    /// enum CmpType { REG_REG, MEM_REG, REG_MEM, CON_REG } cmp_type;

    // We decide the format in advance, for the convenience of register
    // indexing.
    if (is_mem_exp(this->left_)) {
      /// cmp_type = REG_MEM;

      auto in_mem_res = static_cast<tree::MemExp *>(this->left_)
                            ->MunchInMemory(instr_list, fs);
      auto src_regs = in_mem_res.src_regs;
      std::stringstream ss{};
      ss << "cmpq `s" << src_regs->GetList().size() << ", "
         << in_mem_res.operand;
      src_regs->Append(this->right_->Munch(instr_list, fs));
      // CMP instructions have the side effect to modify the machine status.
      instr_list.Append(
          new assem::OperInstr(ss.str(), nullptr, src_regs, nullptr));
      break;
    } else {
      temp::Temp *rhs_reg = nullptr;
      // The right hand of cmp must be register. However, the operand string
      // isn't determined now. The index is known after munch the right exp.
      {
        auto maybe_const_res = this->left_->MunchIfConst(instr_list, fs, false);
        if (maybe_const_res.is_const) {
          // Oops, constant can't be on the right.
          // So we drag it out.
          rhs_reg = this->left_->Munch(instr_list, fs);
        } else {
          rhs_reg = maybe_const_res.u.reg;
        }
      }

      if (is_mem_exp(this->right_)) {
        /// cmp_type = MEM_REG;
        auto in_mem_res = static_cast<tree::MemExp *>(this->right_)
                              ->MunchInMemory(instr_list, fs);
        std::stringstream ss{};
        ss << "cmpq " << in_mem_res.operand << ", `s"
           << in_mem_res.src_regs->GetList().size();
        in_mem_res.src_regs->Append(rhs_reg);
        instr_list.Append(new assem::OperInstr(ss.str(), nullptr,
                                               in_mem_res.src_regs, nullptr));
        break;
      } else {
        auto maybe_const = this->right_->MunchIfConst(instr_list, fs);
        if (maybe_const.is_const) {
          /// cmp_type = CON_REG;
          std::stringstream ss{};
          ss << "cmpq $" << maybe_const.u.val << ", `s0";
          instr_list.Append(new assem::OperInstr(
              ss.str(), nullptr, new temp::TempList{rhs_reg}, nullptr));
          break;
        } else {
          /// cmp_type = REG_REG;
          instr_list.Append(new assem::OperInstr(
              "cmpq `s0, `s1", nullptr,
              new temp::TempList{maybe_const.u.reg, rhs_reg}, nullptr));
          break;
        }
      }
    }
  } while (false);

  std::stringstream ss{};
  ss << op_jmp_mapper.find(this->op_)->second << " `j0";
  auto jump_targets = new std::vector<temp::Label *>{this->true_label_};
  instr_list.Append(new assem::OperInstr(ss.str(), nullptr, nullptr,
                                         new assem::Targets(jump_targets)));
}

void MoveStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  if (typeid(*this->dst_) == typeid(tree::TempExp)) {
    auto reg_dst = static_cast<tree::TempExp *>(this->dst_)->temp_;

    if (typeid(*this->src_) == typeid(tree::MemExp)) {
      auto in_mem_res = static_cast<tree::MemExp *>(this->src_)
                            ->MunchInMemory(instr_list, fs);
      std::stringstream ss{};
      ss << "movq " << in_mem_res.operand << ", `d0";
      instr_list.Append(new assem::OperInstr(
          ss.str(), new temp::TempList{reg_dst}, in_mem_res.src_regs, nullptr));
      return;
    }

    auto maybe_const = this->src_->MunchIfConst(instr_list, fs, false);
    if (maybe_const.is_const) {
      std::stringstream ss{};
      ss << "movq $" << maybe_const.u.val << ", `d0";
      instr_list.Append(
          new assem::MoveInstr(ss.str(), new temp::TempList{reg_dst}, nullptr));
    } else {
      instr_list.Append(
          new assem::MoveInstr("movq `s0, `d0", new temp::TempList{reg_dst},
                               new temp::TempList{maybe_const.u.reg}));
    }
  } else if (typeid(*this->dst_) == typeid(tree::MemExp)) {
    auto in_mem_res =
        static_cast<tree::MemExp *>(this->dst_)->MunchInMemory(instr_list, fs);
    auto src_maybe_const = this->src_->MunchIfConst(instr_list, fs, false);
    if (src_maybe_const.is_const) {
      std::stringstream ss{};
      ss << "movq $" << src_maybe_const.u.val << ", " << in_mem_res.operand;
      instr_list.Append(new assem::OperInstr(ss.str(), nullptr,
                                             in_mem_res.src_regs, nullptr));
    } else {
      std::stringstream ss{};
      auto next_index = in_mem_res.src_regs->GetList().size();
      ss << "movq `s" << next_index << ", " << in_mem_res.operand;
      in_mem_res.src_regs->Append(src_maybe_const.u.reg);
      instr_list.Append(new assem::OperInstr(ss.str(), nullptr,
                                             in_mem_res.src_regs, nullptr));
    }
  } else {
    assert(0);
  }
}

void ExpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  this->exp_->Munch(instr_list, fs);
}

temp::Temp *BinopExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  // Munch further
  auto left_r = this->left_->Munch(instr_list, fs);
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
    // Backup rax, rdx
    auto backup1 = temp::TempFactory::NewTemp();
    auto backup2 = temp::TempFactory::NewTemp();
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{backup1}, new temp::TempList{rax}));
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{backup2}, new temp::TempList{rdx}));
    // Move the lhs to RAX
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{rax}, new temp::TempList{left_r}));
    // Multiply the operand and RAX, place the result in RDX:RAX.
    instr_list.Append(
        new assem::OperInstr("imulq `s0", new temp::TempList{rax, rdx},
                             new temp::TempList{right_r, rax}, nullptr));
    auto r = temp::TempFactory::NewTemp();
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{r}, new temp::TempList{rax}));
    // restore rax, rdx
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{rdx}, new temp::TempList{backup2}));
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{rax}, new temp::TempList{backup1}));
    return r;
  } else if (this->op_ == DIV_OP) {
    // Backup rax, rdx
    auto backup1 = temp::TempFactory::NewTemp();
    auto backup2 = temp::TempFactory::NewTemp();
    auto rax = reg_manager->GetRegister(static_cast<int>(frame::Register::RAX));
    auto rdx = reg_manager->GetRegister(static_cast<int>(frame::Register::RDX));
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{backup1}, new temp::TempList{rax}));
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{backup2}, new temp::TempList{rdx}));
    // Move the dividend to rax
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{rax}, new temp::TempList{left_r}));
    // Extended to 128-bit
    instr_list.Append(new assem::OperInstr("cqto", new temp::TempList{rdx, rax},
                                           new temp::TempList{rax}, nullptr));
    // Divide RDX:RAX by the operand, place the quotient in RAX and the
    // remainder in RDX.
    instr_list.Append(
        new assem::OperInstr("idivq `s0", new temp::TempList{rdx, rax},
                             new temp::TempList{right_r, rax, rdx}, nullptr));
    auto r = temp::TempFactory::NewTemp();
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{r}, new temp::TempList{rax}));
    // restore rax, rdx
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{rdx}, new temp::TempList{backup2}));
    instr_list.Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList{rax}, new temp::TempList{backup1}));
    return r;
  } else {
    assert(0);
  }
  // Normal case
  assert(!i_oper.empty());
  std::stringstream ss{};
  ss << i_oper << " "
     << "`s1, `d0";
  instr_list.Append(new assem::OperInstr(ss.str(), new temp::TempList{left_r},
                                         new temp::TempList{left_r, right_r},
                                         nullptr));
  auto r = temp::TempFactory::NewTemp();
  instr_list.Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList{r},
                                         new temp::TempList{left_r}));
  return r;
}

temp::Temp *MemExp::Munch(assem::InstrList &instr_list, std::string_view fs) {

  auto r = temp::TempFactory::NewTemp();
  auto dst_r = new temp::TempList({r});

  auto mem_res = this->MunchInMemory<InMemoryOperand>(instr_list, fs);
  std::stringstream ss{};
  ss << "movq " << mem_res.operand << ", `d0";
  instr_list.Append(
      new assem::OperInstr(ss.str(), dst_r, mem_res.src_regs, nullptr));
  return r;
}

temp::Temp *TempExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto r = temp::TempFactory::NewTemp();
  instr_list.Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList{r},
                                         new temp::TempList{this->temp_}));
  return r;
}

temp::Temp *EseqExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  this->stm_->Munch(instr_list, fs);
  return this->exp_->Munch(instr_list, fs);
}

temp::Temp *NameExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  // String literal
  auto r = temp::TempFactory::NewTemp();
  std::stringstream ss{};
  ss << "leaq " << temp::LabelFactory::LabelString(this->name_)
     << "(%rip), `d0";
  instr_list.Append(
      new assem::OperInstr(ss.str(), new temp::TempList{r}, nullptr, nullptr));
  return r;
}

temp::Temp *ConstExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  auto r = temp::TempFactory::NewTemp();
  std::stringstream ss{};
  ss << "movq $" << this->consti_ << ", `d0";
  // Yes, it's oper, not move
  instr_list.Append(new assem::OperInstr(ss.str(), new temp::TempList({r}),
                                         nullptr, nullptr));
  return r;
}

temp::Temp *CallExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  assert(typeid(*this->fun_) == typeid(tree::NameExp));

  auto r = temp::TempFactory::NewTemp();
  auto rv = reg_manager->ReturnValue();
  auto args = this->args_->MunchArgs(instr_list, fs);
  auto call_dst = new temp::TempList{rv};
  call_dst->Concat(reg_manager->CallerSaves());

  auto instr_str =
      "callq " + temp::LabelFactory::LabelString(
                     static_cast<tree::NameExp *>(this->fun_)->name_);

  instr_list.Append(new assem::OperInstr(instr_str, call_dst, args, nullptr));

  auto outgoing_cnt = size(this->args_->GetList());
  auto arg_reg_cnt = size(reg_manager->ArgRegs()->GetList());

  // Forget the outgoing arguments. Implemented by increasing the stack pointer.
  if (outgoing_cnt > arg_reg_cnt) {
    auto rsp = reg_manager->StackPointer();
    std::stringstream ss{};
    ss << "addq $" << (outgoing_cnt - arg_reg_cnt) * frame::KX64WordSize
       << ", `d0";
    instr_list.Append(new assem::OperInstr(ss.str(), new temp::TempList{rsp},
                                           new temp::TempList{rsp}, nullptr));
  }

  // The responsibility to move the result to a fresh register.
  instr_list.Append(new assem::MoveInstr("movq `s0, `d0", new temp::TempList{r},
                                         new temp::TempList{rv}));

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
  for (int i = 0; i < std::min(arg_cnt, reg_cnt); ++i, ++arg_exp_it) {
    // Update returned register list
    r_list->Append(convention_args->NthTemp(i));

    tree::Exp *arg_exp = *arg_exp_it;
    auto dst_regs = new temp::TempList({convention_args->NthTemp(i)});

    auto maybe_const = arg_exp->MunchIfConst(instr_list, fs, false);

    if (maybe_const.is_const) {
      std::stringstream ss{};
      ss << "movq $" << maybe_const.u.val << ", `d0";
      auto instr = new assem::MoveInstr(ss.str(), dst_regs, nullptr);
      instr_list.Append(instr);
    } else {
      // Get the argument from another register.
      auto src_regs = new temp::TempList(maybe_const.u.reg);
      auto instr = new assem::MoveInstr("movq `s0, `d0"s, dst_regs, src_regs);
      instr_list.Append(instr);
    }
  }

  std::list<tree::Exp::MaybeConst> param_variant{};
  // Munch the outgoing arguments and store their access,
  // which is used to emit code later.
  for (; arg_exp_it != this->exp_list_.end(); arg_exp_it++) {

    tree::Exp *arg_exp = *arg_exp_it;

    auto maybe_const = arg_exp->MunchIfConst(instr_list, fs, true);
    param_variant.push_back(maybe_const);
  }

  // X86: outgoing parameters are pushed in reverse order.
  auto rsp = reg_manager->StackPointer();
  for (auto it = rbegin(param_variant); it != rend(param_variant); ++it) {

    auto reg_or_const = *it;
    if (reg_or_const.is_const) {
      instr_list.Concat(frame::makePushInstr(reg_or_const.u.val));
    } else {
      instr_list.Concat(frame::makePushInstr(reg_or_const.u.reg));
    }
  }

  return r_list;
}

} // namespace tree
