#include "tiger/escape/escape.h"
#include "tiger/absyn/absyn.h"

namespace esc {
void EscFinder::FindEscape() { absyn_tree_->Traverse(env_.get()); }
} // namespace esc

namespace absyn {

void check_escape_(esc::EscEnvPtr env, sym::Symbol *sym_ptr, int depth) {
  auto look_res = env->Look(sym_ptr);
  if (look_res && !*look_res->escape_ && look_res->depth_ < depth) {
    *look_res->escape_ = true; // In frame
  }
}

void AbsynTree::Traverse(esc::EscEnvPtr env) { this->root_->Traverse(env, 0); }

void SimpleVar::Traverse(esc::EscEnvPtr env, int depth) {
  check_escape_(env, this->sym_, depth);
}

void FieldVar::Traverse(esc::EscEnvPtr env, int depth) {
  this->var_->Traverse(env, depth);
}

void SubscriptVar::Traverse(esc::EscEnvPtr env, int depth) {
  this->var_->Traverse(env, depth);
  this->subscript_->Traverse(env, depth);
}

void VarExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->var_->Traverse(env, depth);
}

void NilExp::Traverse(esc::EscEnvPtr env, int depth) {}

void IntExp::Traverse(esc::EscEnvPtr env, int depth) {}

void StringExp::Traverse(esc::EscEnvPtr env, int depth) {}

void CallExp::Traverse(esc::EscEnvPtr env, int depth) {
  for (auto arg : this->args_->GetList()) {
    arg->Traverse(env, depth);
  }
}

void OpExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->left_->Traverse(env, depth);
  this->right_->Traverse(env, depth);
}

void RecordExp::Traverse(esc::EscEnvPtr env, int depth) {
  for (auto field : this->fields_->GetList()) {
    field->exp_->Traverse(env, depth);
  }
}

void SeqExp::Traverse(esc::EscEnvPtr env, int depth) {
  for (auto exp : this->seq_->GetList()) {
    exp->Traverse(env, depth);
  }
}

void AssignExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->var_->Traverse(env, depth);
  this->exp_->Traverse(env, depth);
}

void IfExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->test_->Traverse(env, depth);
  this->then_->Traverse(env, depth);
  if (this->elsee_) {
    this->elsee_->Traverse(env, depth);
  }
}

void WhileExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->test_->Traverse(env, depth);
  this->body_->Traverse(env, depth);
}

void ForExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->lo_->Traverse(env, depth);
  this->hi_->Traverse(env, depth);
  // env->BeginScope();
  // env->Enter(this->var_, new esc::EscapeEntry(depth + 1, &this->escape_));
  this->body_->Traverse(env, depth);
  // env->EndScope();
}

void BreakExp::Traverse(esc::EscEnvPtr env, int depth) {}

void LetExp::Traverse(esc::EscEnvPtr env, int depth) {
  env->BeginScope();
  for (auto dec : this->decs_->GetList()) {
    dec->Traverse(env, depth);
  }
  this->body_->Traverse(env, depth);
  env->EndScope();
}

void ArrayExp::Traverse(esc::EscEnvPtr env, int depth) {
  this->size_->Traverse(env, depth);
}

void VoidExp::Traverse(esc::EscEnvPtr env, int depth) {}

void FunctionDec::Traverse(esc::EscEnvPtr env, int depth) {
  for (auto fun_dec : this->functions_->GetList()) {
    env->BeginScope();
    for (auto arg : fun_dec->params_->GetList()) {
      env->Enter(arg->name_, new esc::EscapeEntry(depth + 1, &arg->escape_));
    }
    fun_dec->body_->Traverse(env, depth + 1);
    env->EndScope();
  }
}

void VarDec::Traverse(esc::EscEnvPtr env, int depth) {
  env->Enter(this->var_, new esc::EscapeEntry(depth, &this->escape_));
}

void TypeDec::Traverse(esc::EscEnvPtr env, int depth) {}

} // namespace absyn
