#include "straightline/slp.h"

#include <algorithm>
#include <cassert>

namespace A {
int A::CompoundStm::MaxArgs() const {
  return std::max(this->stm1->MaxArgs(), this->stm2->MaxArgs());
}

Table *A::CompoundStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
}

int A::AssignStm::MaxArgs() const {
  return this->exp->MaxArgs();
}

Table *A::AssignStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
}

int A::PrintStm::MaxArgs() const {
  return std::max(this->exps->get_exp_number(), this->exps->MaxArgs());
}

Table *A::PrintStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
}

int Table::Lookup(const std::string &key) const {
  if (id == key) {
    return value;
  } else if (tail != nullptr) {
    return tail->Lookup(key);
  } else {
    assert(false);
  }
}

Table *Table::Update(const std::string &key, int val) const {
  // return new Table(key, val, this);
}

int A::LastExpList::get_exp_number() const {
  return 1;
}

int A::PairExpList::get_exp_number() const {
  return 1 + this->tail->get_exp_number();
}

int A::IdExp::MaxArgs() const {
  return 0;
}

int A::NumExp::MaxArgs() const {
  return 0;
}

int A::OpExp::MaxArgs() const {
  return std::max(this->left->MaxArgs(), this->right->MaxArgs());
}

int A::EseqExp::MaxArgs() const {
  return std::max(this->stm->MaxArgs(), this->exp->MaxArgs());
}

int A::PairExpList::MaxArgs() const {
  return std::max(this->exp->MaxArgs(), this->tail->MaxArgs());
}

int A::LastExpList::MaxArgs() const {
  return this->exp->MaxArgs();
}

} // namespace A
