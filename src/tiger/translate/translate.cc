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
  [[nodiscard]] virtual tree::Exp *UnEx() const = 0;
  [[nodiscard]] virtual tree::Stm *UnNx() const = 0;
  [[nodiscard]] virtual Cx UnCx(err::ErrorMsg *errormsg) const = 0;

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

  [[nodiscard]] tree::Exp *UnEx() const override { return this->exp_; }
  [[nodiscard]] tree::Stm *UnNx() const override {
    /* TODO: Put your lab5 code here */
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) const override {
    /* TODO: Put your lab5 code here */
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

  [[nodiscard]] tree::Exp *UnEx() const override {
    /* TODO: Put your lab5 code here */
  }
  [[nodiscard]] tree::Stm *UnNx() const override { return this->stm_; }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) const override {
    /* TODO: Put your lab5 code here */
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

  [[nodiscard]] tree::Exp *UnEx() const override {
    /* TODO: Put your lab5 code here */
  }
  [[nodiscard]] tree::Stm *UnNx() const override {
    /* TODO: Put your lab5 code here */
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) const override {
    /* TODO: Put your lab5 code here */
  }
};

// TODO is this reasonable? (share a global no_op exp?)
Exp *Exp::no_op() {
  static ExExp no_op_exp = tr::ExExp(new tree::ConstExp(0));
  return &no_op_exp;
}

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
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *done,
                               err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

tr::ExpAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *done,
                                  err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab5 code here */
}

/**
 * @brief Pseudocode of the for expression:
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
 * @param venv
 * @param tenv
 * @param level
 * @param label
 * @param errormsg
 * @return tr::ExpAndTy*
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
