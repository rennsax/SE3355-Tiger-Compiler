#include "tiger/translate/translate.h"

#include <tiger/absyn/absyn.h>

#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/frame/x64frame.h"

/**
 * All the remembered fragments go into a private fragment list within
 * Translate.
 */
extern frame::Frags *frags;
extern frame::RegManager *reg_manager;

namespace tr {

// [[deprecated("not used")]]
static constexpr type::Ty *dummyType = nullptr;

struct Cx {
  PatchList trues_;
  PatchList falses_;
  tree::Stm *stm_;

  Cx(PatchList trues, PatchList falses, tree::Stm *stm)
      : trues_(trues), falses_(falses), stm_(stm) {}
};

/**
 * @brief Interface for tr::ExExp, tr::NxExp and tr::CxExp.
 *
 * The virtual class defines three converting functions, which are useful to
 * "unwrap" the data structures defined in `translate' module and result in
 * the converting results.
 * From the textbook (P156), the three conversion functions behave like simply
 * "stripping off the corresponding constructor". However, they should work no
 * matter what constructor has been used!
 *
 * @note It's an abstract class. Its derived classes (ExExp, NxExp, CxExp) are
 * visible only in the translate module, therefore declared in translate.cc.
 *
 * @example (P156)
 * Suppose we have a tr::CxExp e which represents `a>b|c<d`:
 *    e = new tr::CxExp(trues, falses, stm);
 * and we have an assignment statement to translate:
 *    flag := (a>b|c<d);
 * Then we can implement the assignment as:
 *    MOVE(TEMP_{flag}, e->UnEx());
 * Even though CxExp is there instead.
 *
 */
class Exp {
public:
  [[nodiscard]] virtual tree::Exp *UnEx() = 0;
  [[nodiscard]] virtual tree::Stm *UnNx() = 0;
  [[nodiscard]] virtual Cx UnCx(err::ErrorMsg *errormsg) = 0;

  virtual ~Exp() = default;

  /**
   * @brief Return a no_op expression as a placeholder for further usage.
   *
   * Use case example: returned in FunctionDec#Translate.
   *
   * @return Exp* A no_op expression.
   */
  static Exp *no_op();
};

/**
 * @brief A struct consists of IR expression of a Tiger expression and type
 * information.
 *
 * While type-checking, we only have to return `type::Ty*` after semantic
 * analysis. But now, `Translate` must return a new data structure `ExpAndTy`,
 * which includes the intermediate-representation translation of each Tiger
 * expression.
 *
 * @note The type is used as the return type of Translate interface in Lab5.
 * And since we have done type checking in the Traverse interface (Lab4),
 * factually we won't use the filed @c ty_ at all.
 * Therefore, returning such data structure is redundant.
 */
struct ExpAndTy {
  tr::Exp *exp_;
  type::Ty *ty_;

  ExpAndTy(tr::Exp *exp) : exp_(exp), ty_(dummyType) {}
  ExpAndTy(tr::Exp *exp, type::Ty *ty) : exp_(exp), ty_(ty) {}
};

struct ExExp : public Exp {
  tree::Exp *exp_;

  /**
   * @brief An expression.
   *
   * @param exp
   */
  explicit ExExp(tree::Exp *exp) : exp_(exp) {}

  [[nodiscard]] tree::Exp *UnEx() override { return this->exp_; }
  [[nodiscard]] tree::Stm *UnNx() override {
    // ExpStm
    return new tree::ExpStm(this->exp_);
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    auto t = temp::LabelFactory::NewLabel();
    auto f = temp::LabelFactory::NewLabel();
    auto trues = PatchList({&t});
    auto falses = PatchList({&f});
    tree::Stm *cjump_stm = new tree::CjumpStm(tree::RelOp::NE_OP, this->exp_,
                                              new tree::ConstExp(0), t, f);
    return Cx(trues, falses, cjump_stm);
  }
};

struct NxExp : public Exp {
  tree::Stm *stm_;

  /**
   * @brief "no result", which corresponds a common statement of tree language.
   *
   * @param stm
   */
  explicit NxExp(tree::Stm *stm) : stm_(stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    // Never occur
    assert(0);
  }
  [[nodiscard]] tree::Stm *UnNx() override { return this->stm_; }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    // Never occur
    assert(0);
  }
};

struct CxExp : public Exp {
  Cx cx_;

  /**
   * @brief Conditional. It can be regarded as a wrapper of tr::Cx.
   *
   * In essence, create a tr::Cx with the arguments.
   *
   * @param trues the patch list
   * @param falses
   * @param stm the statement may jump to the true-label or the false-label, but
   * these labels have yet to be filled in.
   */
  CxExp(PatchList trues, PatchList falses, tree::Stm *stm)
      : cx_(trues, falses, stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    // The finial result.
    auto r = temp::TempFactory::NewTemp();
    auto t = temp::LabelFactory::NewLabel();
    auto f = temp::LabelFactory::NewLabel();
    this->cx_.trues_.DoPatch(t);
    this->cx_.falses_.DoPatch(f);
    return new tree::EseqExp(
        new tree::MoveStm(new tree::TempExp(r), new tree::ConstExp(1)),
        new tree::EseqExp(
            this->cx_.stm_,
            new tree::EseqExp(
                new tree::LabelStm(f),
                new tree::EseqExp(new tree::MoveStm(new tree::TempExp(r),
                                                    new tree::ConstExp(0)),
                                  new tree::EseqExp(new tree::LabelStm(t),
                                                    new tree::TempExp(r))))));
  }
  [[nodiscard]] tree::Stm *UnNx() override {
    // Unknown
    assert(0);
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override { return this->cx_; }
};

Exp *Exp::no_op() { return new tr::ExExp(new tree::ConstExp(0)); }

void ProgTr::Translate() {
  FillBaseTEnv();
  FillBaseVEnv();
  this->absyn_tree_->Translate(this->venv_.get(), this->tenv_.get(),
                               this->main_level_.get(), nullptr,
                               errormsg_.get());
}

tr::Level *tr::Level::newBaseLevel(temp::Label *name) {
  auto level = new Level();
  level->frame_ = frame::newFrame(name, {});
  level->parent_ = nullptr;
  return level;
}

tr::Access *tr::Level::allocLocal(bool escape) {
  auto raw_access = this->frame_->allocateLocal(escape);
  return new tr::Access(this, raw_access);
}

frame::Access *tr::Level::stackLink() const {
  return this->frame_->getFormals().front();
}

/**
 * @brief Utility function to follow static link.
 * More details: P160.
 *
 */
tr::Exp *makeSimpleVariable(tr::Access *access, tr::Level *level,
                            tree::Exp *framePointer) {
  if (access->level_ == level) {
    return new tr::ExExp(access->access_->toExp(framePointer));
  }
  // Follow static links, recursively.
  // If the compiler is well-defined, the access must not be escaped here.

  // Get the static link, which is the frame pointer of the next level.
  assert(level->parent_);
  auto fp_new = level->stackLink()->toExp(framePointer);
  return makeSimpleVariable(access, level->parent_, fp_new);
}

tr::Exp *makeSimpleVariable(tr::Access *access, tr::Level *level) {
  // Initial frame pointer
  auto fp = reg_manager->FramePointer();
  return makeSimpleVariable(access, level, new tree::TempExp(fp));
}

tr::Exp *makeBinaryExp(absyn::Oper oper, tr::Exp *left, tr::Exp *right) {
  using absyn::Oper;
  static std::unordered_map<Oper, tree::BinOp> arithmetic_op_mapper = {
      // AND and OR need to be treated specially.
      {Oper::PLUS_OP, tree::BinOp::PLUS_OP},
      {Oper::MINUS_OP, tree::BinOp::MINUS_OP},
      {Oper::TIMES_OP, tree::BinOp::MUL_OP},
      {Oper::DIVIDE_OP, tree::BinOp::DIV_OP},
  };
  static std::unordered_map<Oper, tree::RelOp> relation_op_mapper = {
      {Oper::EQ_OP, tree::RelOp::EQ_OP}, {Oper::NEQ_OP, tree::RelOp::NE_OP},
      {Oper::LE_OP, tree::RelOp::LE_OP}, {Oper::LT_OP, tree::RelOp::LT_OP},
      {Oper::GE_OP, tree::RelOp::GE_OP}, {Oper::GT_OP, tree::RelOp::GT_OP},
  };

  if (auto arith_op_it = arithmetic_op_mapper.find(oper);
      arith_op_it != arithmetic_op_mapper.end()) {
    return new tr::ExExp(
        new tree::BinopExp(arith_op_it->second, left->UnEx(), right->UnEx()));
  } else if (auto rel_op_it = relation_op_mapper.find(oper);
             rel_op_it != relation_op_mapper.end()) {
    auto cjump_stm = new tree::CjumpStm(rel_op_it->second, left->UnEx(),
                                        right->UnEx(), nullptr, nullptr);
    auto trues = tr::PatchList({&cjump_stm->true_label_});
    auto falses = tr::PatchList({&cjump_stm->false_label_});
    return new tr::CxExp(trues, falses, cjump_stm);
  }

  // Handle short circuit
  if (oper == Oper::AND_OP) {
    // left & right => if left then right else 0
    return tr::makeIfThenElse(left, right, new tr::ExExp(new tree::ConstExp(0)),
                              nullptr); // FIXME
  } else if (oper == Oper::OR_OP) {
    // left | right => if left then 1 else right
    return tr::makeIfThenElse(left, new tr::ExExp(new tree::ConstExp(1)), right,
                              nullptr); // FIXME
  }

  assert(0);
}

tr::Exp *makeSequentialExp(std::list<tr::Exp *> expList) {
  if (expList.empty()) {
    assert(0);
    return tr::Exp::no_op();
  }

  auto first = expList.front();
  expList.pop_front();
  if (expList.empty()) {
    return first;
  }
  auto other_res = makeSequentialExp(std::move(expList));
  auto other_exp = other_res->UnEx();
  delete other_res;

  return new tr::ExExp(
      new tree::EseqExp(new tree::ExpStm(first->UnEx()), other_exp));
}

[[nodiscard]] tr::Exp *makeIfThenElse(tr::Exp *test_e, tr::Exp *then_e,
                                      tr::Exp *else_e, err::ErrorMsg *err_msg) {
  // FIXME not efficient? (P165)
  // A new register to store the result.
  auto result_temp = temp::TempFactory::NewTemp();
  auto cjump_stm = test_e->UnCx(err_msg);
  tree::Stm *true_action = nullptr;
  tree::Stm *false_action = nullptr;
  if (else_e == nullptr) {
    true_action = then_e->UnNx();
    false_action = tr::Exp::no_op()->UnNx();
  } else {
    true_action =
        new tree::MoveStm(new tree::TempExp(result_temp), then_e->UnEx());
    false_action =
        new tree::MoveStm(new tree::TempExp(result_temp), else_e->UnEx());
  }
  assert(true_action && false_action);

  auto t = temp::LabelFactory::NewLabel();
  auto f = temp::LabelFactory::NewLabel();
  cjump_stm.trues_.DoPatch(t);
  cjump_stm.falses_.DoPatch(f);

  if (else_e == nullptr) {
    return new tr::NxExp(new tree::SeqStm(
        cjump_stm.stm_,
        new tree::SeqStm(
            new tree::LabelStm(f),
            new tree::SeqStm(
                false_action,
                new tree::SeqStm(new tree::LabelStm(t), true_action)))));
  } else {
    return new tr::ExExp(new tree::EseqExp(
        cjump_stm.stm_,
        new tree::EseqExp(
            new tree::LabelStm(f),
            new tree::EseqExp(
                false_action,
                new tree::EseqExp(
                    new tree::LabelStm(t),
                    new tree::EseqExp(true_action,
                                      new tree::TempExp(result_temp)))))));
  }
}

} // namespace tr

// Translation is done in the semantic analysis phase of Tiger compiler.
namespace absyn {

tr::ExpAndTy *AbsynTree::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();
  auto res = this->root_->Translate(venv, tenv, level, done, errormsg);
  tenv->EndScope();
  venv->EndScope();
  return res;
}

tr::ExpAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  auto entry = venv->Look(this->sym_);
  assert(entry);
  assert(typeid(*entry) == typeid(env::VarEntry));
  auto var_entry = static_cast<env::VarEntry *>(entry);
  auto exp = tr::makeSimpleVariable(var_entry->access_, level);
  return new tr::ExpAndTy(exp, var_entry->ty_);
}

tr::ExpAndTy *FieldVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *done,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *SubscriptVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                      tr::Level *level, temp::Label *done,
                                      err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *VarExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  auto var_exp = this->var_->Translate(venv, tenv, level, done, errormsg);
  return new tr::ExpAndTy(var_exp->exp_);
}

tr::ExpAndTy *NilExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *IntExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(this->val_)),
                          tr::dummyType);
}

tr::ExpAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *CallExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *done,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *done,
                               err::ErrorMsg *errormsg) const {
  auto left_exp_and_ty =
      this->left_->Translate(venv, tenv, level, done, errormsg);
  auto right_exp_and_ty =
      this->right_->Translate(venv, tenv, level, done, errormsg);

  auto res_exp = tr::makeBinaryExp(this->oper_, left_exp_and_ty->exp_,
                                   right_exp_and_ty->exp_);
  return new tr::ExpAndTy(res_exp, tr::dummyType);
}

tr::ExpAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  std::list<tr::Exp *> expList{};

  assert(!this->seq_->GetList().empty());
  for (auto exp : this->seq_->GetList()) {
    auto ret = exp->Translate(venv, tenv, level, done, errormsg);
    expList.push_back(ret->exp_);
  }
  auto res_exp = tr::makeSequentialExp(expList);
  return new tr::ExpAndTy(res_exp);
}

tr::ExpAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *done,
                               err::ErrorMsg *errormsg) const {
  auto test_exp_and_ty =
      this->test_->Translate(venv, tenv, level, done, errormsg);
  auto then_exp_and_ty =
      this->then_->Translate(venv, tenv, level, done, errormsg);

  auto else_exp_and_ty =
      this->elsee_ == nullptr
          ? nullptr
          : this->elsee_->Translate(venv, tenv, level, done, errormsg);

  auto res_exp = tr::makeIfThenElse(
      test_exp_and_ty->exp_, then_exp_and_ty->exp_,
      (else_exp_and_ty == nullptr ? nullptr : else_exp_and_ty->exp_), errormsg);

  return new tr::ExpAndTy(res_exp);
}

tr::ExpAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *done,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

/**
 * Pseudocode of the for expression:
 *
 * ```
 * if i > limit goto done
 *    body
 * if i == limit goto done
 * Loop:
 *    i := i + 1
 *    body
 *    if i <= limit goto Loop
 * done:
 * ```
 *
 */
tr::ExpAndTy *ForExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *BreakExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *done,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *LetExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();
  for (auto dec : this->decs_->GetList()) {
    dec->Translate(venv, tenv, level, done, errormsg);
  }
  auto res = this->body_->Translate(venv, tenv, level, done, errormsg);
  venv->EndScope();
  tenv->EndScope();
  return res;
}

tr::ExpAndTy *ArrayExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *done,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *VoidExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *done,
                                 err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

/**
 * @brief Translate a Tiger function definition.
 *
 * (P173)
 * 1. Call upon tr::Level::newLevel in processing a function header.
 * 2. Call other interfaces in tr to translate the function body, which has
 *    the side effect of remembering fragments for any string literals.
 * 3. Call tr::procEntryExit, which has the side effect of remembering a
 *    ProgFrag.
 *
 * All the remembered fragments go into a private fragment list within
 * Translate.
 *
 * @param venv
 * @param tenv
 * @param level
 * @param label
 * @param errormsg
 * @return tr::Exp*
 */
tr::Exp *FunctionDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
  return tr::Exp::no_op();
}

tr::Exp *VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                           tr::Level *level, temp::Label *done,
                           err::ErrorMsg *errormsg) const {
  // var decl in Tiger language must give an init.
  assert(this->init_);
  auto init_exp_and_ty =
      this->init_->Translate(venv, tenv, level, done, errormsg);

  // Allocate a local variable in the frame (increase the frame size).
  auto access = level->allocLocal(this->escape_);
  auto dst_exp = tr::makeSimpleVariable(access, level);

  // Update var env
  venv->Enter(this->var_, new env::VarEntry(access, tr::dummyType));
  auto init_exp = new tr::NxExp(
      new tree::MoveStm(dst_exp->UnEx(), init_exp_and_ty->exp_->UnEx()));
  return init_exp;
}

tr::Exp *TypeDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                            tr::Level *level, temp::Label *done,
                            err::ErrorMsg *errormsg) const {
  return tr::Exp::no_op();
}

type::Ty *NameTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

type::Ty *RecordTy::Translate(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

type::Ty *ArrayTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

} // namespace absyn
