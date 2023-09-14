#include "straightline/slp.h"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <tuple>

namespace A {
int A::CompoundStm::MaxArgs() const {
  return std::max(this->stm1->MaxArgs(), this->stm2->MaxArgs());
}

int A::AssignStm::MaxArgs() const { return this->exp->MaxArgs(); }

int A::PrintStm::MaxArgs() const {
  return std::max(this->exps->get_exp_number(), this->exps->MaxArgs());
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
  return new Table(key, val, this);
}

int A::LastExpList::get_exp_number() const { return 1; }

int A::PairExpList::get_exp_number() const {
  return 1 + this->tail->get_exp_number();
}

int A::IdExp::MaxArgs() const { return 0; }

int A::NumExp::MaxArgs() const { return 0; }

int A::OpExp::MaxArgs() const {
  return std::max(this->left->MaxArgs(), this->right->MaxArgs());
}

int A::EseqExp::MaxArgs() const {
  return std::max(this->stm->MaxArgs(), this->exp->MaxArgs());
}

int A::PairExpList::MaxArgs() const {
  return std::max(this->exp->MaxArgs(), this->tail->MaxArgs());
}

int A::LastExpList::MaxArgs() const { return this->exp->MaxArgs(); }

Table *A::PrintStm::Interp(Table *table) const {
  return this->exps->InterpByPrintStm(table);
}

Table *A::AssignStm::Interp(Table *table) const {
  auto int_and_table = this->exp->Interp(table);
  return int_and_table->t->Update(this->id, int_and_table->i);
}

Table *A::CompoundStm::Interp(Table *table) const {
  return this->stm2->Interp(this->stm1->Interp(table));
}

IntAndTable *A::IdExp::Interp(Table *table) const {
  // no side effects
  return new IntAndTable(table->Lookup(this->id), table);
};
IntAndTable *A::NumExp::Interp(Table *table) const {
  // no side effects
  return new IntAndTable(this->num, table);
};
IntAndTable *A::EseqExp::Interp(Table *table) const {
  // may produce side effects
  return this->exp->Interp(this->stm->Interp(table));
};
IntAndTable *A::OpExp::Interp(Table *table) const {
  constexpr auto op = [](int a, BinOp op, int b) -> int {
    switch (op) {
    case PLUS:
      return a + b;
    case MINUS:
      return a - b;
    case TIMES:
      return a * b;
    case DIV:
      return a / b;
    }
  };
  auto left_int_and_table = this->left->Interp(table);
  auto right_int_and_table = this->right->Interp(left_int_and_table->t);
  return new IntAndTable(
      std::apply(op, std::make_tuple(left_int_and_table->i, this->oper,
                                     right_int_and_table->i)),
      right_int_and_table->t);
};

Table *A::LastExpList::InterpByPrintStm(Table *table) const {
  auto int_and_table = this->exp->Interp(table);
  std::cout << int_and_table->i << std::endl;
  return int_and_table->t;
}

Table *A::PairExpList::InterpByPrintStm(Table *table) const {
  auto int_and_table = this->exp->Interp(table);
  std::cout << int_and_table->i << '\x20';
  return this->tail->InterpByPrintStm(int_and_table->t);
}

} // namespace A
