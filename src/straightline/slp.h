#ifndef STRAIGHTLINE_SLP_H_
#define STRAIGHTLINE_SLP_H_

#include <string>

namespace A {

class Stm;
class Exp;
class ExpList;

enum BinOp { PLUS = 0, MINUS, TIMES, DIV };

// some data structures used by interp
class Table;
class IntAndTable;

class Stm {
public:
  virtual int MaxArgs() const = 0;
  virtual Table *Interp(Table *) const = 0;
};

class CompoundStm : public Stm {
public:
  CompoundStm(Stm *stm1, Stm *stm2) : stm1(stm1), stm2(stm2) {}
  int MaxArgs() const override;
  Table *Interp(Table *) const override;

private:
  Stm *stm1, *stm2;
};

class AssignStm : public Stm {
public:
  AssignStm(std::string id, Exp *exp) : id(std::move(id)), exp(exp) {}
  int MaxArgs() const override;
  Table *Interp(Table *) const override;

private:
  std::string id;
  Exp *exp;
};

class PrintStm : public Stm {
public:
  explicit PrintStm(ExpList *exps) : exps(exps) {}
  int MaxArgs() const override;
  Table *Interp(Table *) const override;

private:
  ExpList *exps;
};

class Exp {
public:
  virtual int MaxArgs() const = 0;
  virtual IntAndTable *Interp(Table *) const = 0;
};

class IdExp : public Exp {
public:
  explicit IdExp(std::string id) : id(std::move(id)) {}
  int MaxArgs() const override;
  IntAndTable *Interp(Table *) const override;

private:
  std::string id;
};

class NumExp : public Exp {
public:
  explicit NumExp(int num) : num(num) {}
  int MaxArgs() const override;
  IntAndTable *Interp(Table *) const override;

private:
  int num;
};

class OpExp : public Exp {
public:
  OpExp(Exp *left, BinOp oper, Exp *right)
      : left(left), oper(oper), right(right) {}
  int MaxArgs() const override;
  IntAndTable *Interp(Table *) const override;

private:
  Exp *left;
  BinOp oper;
  Exp *right;
};

/**
 * @brief Expression Sequence.
 * For example, (s, e) where `s` may produce side effects.
 */
class EseqExp : public Exp {
public:
  EseqExp(Stm *stm, Exp *exp) : stm(stm), exp(exp) {}
  int MaxArgs() const override;
  IntAndTable *Interp(Table *) const override;

private:
  Stm *stm;
  Exp *exp;
};

/**
 * @brief Expression List Interface
 * @attention ExpList is only used in PrintStm
 */
class ExpList {
public:
  virtual int get_exp_number() const = 0;
  virtual int MaxArgs() const = 0;
  virtual Table *InterpByPrintStm(Table *) const = 0;
};

class PairExpList : public ExpList {
public:
  PairExpList(Exp *exp, ExpList *tail) : exp(exp), tail(tail) {}
  int get_exp_number() const override;
  int MaxArgs() const override;
  Table *InterpByPrintStm(Table *) const override;

private:
  Exp *exp;
  ExpList *tail;
};

class LastExpList : public ExpList {
  friend Table *Stm::Interp(Table *) const;

public:
  LastExpList(Exp *exp) : exp(exp) {}
  int get_exp_number() const override;
  int MaxArgs() const override;

private:
  Table *InterpByPrintStm(Table *) const override;
  Exp *exp;
};

class Table {
public:
  Table(std::string id, int value, const Table *tail)
      : id(std::move(id)), value(value), tail(tail) {}
  int Lookup(const std::string &key) const;
  Table *Update(const std::string &key, int val) const;

private:
  std::string id;
  int value;
  const Table *tail;
};

struct IntAndTable {
  int i;
  Table *t;

  IntAndTable(int i, Table *t) : i(i), t(t) {}
};

} // namespace A

#endif // STRAIGHTLINE_SLP_H_
