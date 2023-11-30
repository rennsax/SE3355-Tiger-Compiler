/**
 * @file tree.h
 * @brief the intermediate representation (IR) tree language
 *
 * Suitable for Tiger.
 *
 */
#ifndef TIGER_TRANSLATE_TREE_H_
#define TIGER_TRANSLATE_TREE_H_

#include <array>
#include <cassert>
#include <cstdio>
#include <list>
#include <string>

#include "tiger/frame/temp.h"

// Forward Declarations
namespace canon {
class StmAndExp;
class Canon;
} // namespace canon

namespace assem {
class InstrList;
} // namespace assem

namespace frame {
class RegManager;
} // namespace frame

namespace tree {

class Stm;
class Exp;
class NameExp;

class ExpList;
class StmList;

enum BinOp {
  PLUS_OP,
  MINUS_OP,
  MUL_OP,
  DIV_OP,
  AND_OP,
  OR_OP,
  LSHIFT_OP,  // logical
  RSHIFT_OP,  // logical
  ARSHIFT_OP, // arithmetical
  XOR_OP,
  BIN_OPER_COUNT,
};

enum RelOp {
  EQ_OP,
  NE_OP,
  LT_OP,
  GT_OP,
  LE_OP,
  GE_OP,
  ULT_OP,
  ULE_OP,
  UGT_OP,
  UGE_OP,
  REL_OPER_COUNT,
};

/**
 * Statements
 */

class Stm {
public:
  virtual ~Stm() = default;

  virtual void Print(FILE *out, int d) const = 0;
  /**
   * @brief Auxiliary function called by reorder.
   *
   * Corresponds to `do_stm` on the textbook (P182).
   * Pulls all the ESEQ s out of a statement.
   *
   * @return Stm*
   */
  virtual Stm *Canon() = 0;

  /**
   * @brief Maximal munch.
   *
   * See in Exp::Munch.
   *
   * @param instr_list
   * @param fs
   */
  virtual void Munch(assem::InstrList &instr_list, std::string_view fs) = 0;

  // Used for Canon
  bool IsNop();
  /**
   * @brief Combine two statements.
   *
   * x and y may be no-op, which would be handled.
   *
   * @param x
   * @param y
   * @return Stm* some statement _equivalent_ to SEQ(s1, s2).
   */
  static Stm *Seq(Stm *x, Stm *y);
  /**
   * @brief Determine whether x and y "definitely" commute.
   *
   * It's naive. We just say if y is constant / label or x is "no-op", then
   * they commute.
   *
   * @param x
   * @param y
   * @return bool true is x and y commute
   */
  static bool Commute(tree::Stm *x, tree::Exp *y);
};

class SeqStm : public Stm {
public:
  Stm *left_, *right_;

  /**
   * @brief Two sequencial statements.
   *
   * @param left
   * @param right
   */
  SeqStm(Stm *left, Stm *right) : left_(left), right_(right) { assert(left); }
  ~SeqStm() override;

  void Print(FILE *out, int d) const override;
  Stm *Canon() override;
  void Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class LabelStm : public Stm {
public:
  temp::Label *label_;

  /**
   * @brief Define the constant value of name label to be the current machine
   * code address. This is like a label definition in assembly language.
   *
   * @param label
   */
  explicit LabelStm(temp::Label *label) : label_(label) {}
  ~LabelStm() override;

  void Print(FILE *out, int d) const override;
  Stm *Canon() override;
  void Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class JumpStm : public Stm {
public:
  NameExp *exp_;
  std::vector<temp::Label *> *jumps_;

  /**
   * @brief Jump to address evaluated by exp.
   *
   * @example Jump to a known label l: JumpStm(l, {l});
   *
   * @param exp evaluated for the address
   * @param jumps The list of labels labs specifies all the possible locations
   * that the expression e can evaluate to; this is necessary for dataflow
   * analysis later.
   */
  JumpStm(NameExp *exp, std::vector<temp::Label *> *jumps)
      : exp_(exp), jumps_(jumps) {}
  ~JumpStm() override;

  void Print(FILE *out, int d) const override;
  Stm *Canon() override;
  void Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class CjumpStm : public Stm {
public:
  RelOp op_;
  Exp *left_, *right_;
  temp::Label *true_label_, *false_label_;

  /**
   * @brief Conditional jump.
   *
   * The workflow: a <- left, b <- right, if_mov <- a op b, jump to true_label
   * if if_mov == true, otherwise jump to false_label.
   *
   * @param op with which relational operator the results are compared.
   * @param left the first expression to evaluate
   * @param right the second expression to evaluate
   * @param true_label
   * @param false_label
   */
  CjumpStm(RelOp op, Exp *left, Exp *right, temp::Label *true_label,
           temp::Label *false_label)
      : op_(op), left_(left), right_(right), true_label_(true_label),
        false_label_(false_label) {}
  ~CjumpStm() override;

  void Print(FILE *out, int d) const override;
  Stm *Canon() override;
  void Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class MoveStm : public Stm {
public:
  Exp *dst_, *src_;

  /**
   * @brief Move statement.
   *
   * Type 1: MOVE(TEMP t, e) Evaluate e and move it into t.
   * Type 2: MOVE(MEM(e1), e2) Evaluate e1, yielding address a. Then evaluate
   * e2, and store the result into wordSize bytes of memory starting at a.
   *
   * @param dst
   * @param src
   */
  MoveStm(Exp *dst, Exp *src) : dst_(dst), src_(src) {}
  ~MoveStm() override;

  void Print(FILE *out, int d) const override;
  Stm *Canon() override;
  void Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class ExpStm : public Stm {
public:
  Exp *exp_;

  /**
   * @brief Evaluate exp and discard the result.
   *
   * @param exp
   */
  explicit ExpStm(Exp *exp) : exp_(exp) {}
  ~ExpStm() override;

  void Print(FILE *out, int d) const override;
  Stm *Canon() override;
  void Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

/**
 *Expressions
 */

class Exp {
public:
  virtual ~Exp() = default;

  virtual void Print(FILE *out, int d) const = 0;
  /**
   * @brief Auxiliary function called by reorder.
   *
   * It corresponds to `do_exp` on the textbook.
   *
   * @return canon::StmAndExp
   * A statement s and an expression e', where e' contains no ESEQs, such that
   * ESEQ(s, e') would be equivalent to the original expression e.
   */
  virtual canon::StmAndExp Canon() = 0;

  /**
   * @brief Maximal munch, an algorithm for optimal (locally the best) tiling.
   *
   * The algorithm is described on the textbook P195:
   * Starting at the root of the tree, find the largest tile that fits. Cover
   * the root node – and perhaps several other nodes near the root – with this
   * tile, leaving several subtrees. Now repeat the same algorithm for each
   * subtree.
   * A tile matches if each nonleaf node of the tile is labeled with the same
   * operator as the corresponding node of the tree.
   *
   * @param instr_list The list to accumulate the instructions.
   * @param fs // TODO what's this?
   * @return temp::Temp The IR expression is evaluated, and the result (on the
   * node) is returned.
   */
  virtual temp::Temp *Munch(assem::InstrList &instr_list,
                            std::string_view fs) = 0;
};

class BinopExp : public Exp {
public:
  BinOp op_;
  Exp *left_, *right_;

  /**
   * @brief Binary operation op to operand left, right.
   *
   * Subexpression left is evaluated before right.
   *
   * @param op
   * @param left
   * @param right
   */
  BinopExp(BinOp op, Exp *left, Exp *right)
      : op_(op), left_(left), right_(right) {}
  ~BinopExp() override;

  void Print(FILE *out, int d) const override;
  canon::StmAndExp Canon() override;
  temp::Temp *Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class MemExp : public Exp {
public:
  Exp *exp_;

  /**
   * @brief the contents of `wordSize` bytes of memory starting at address `exp`
   *
   * `wordSize` is defined by the virtual function
   * `frame::RegManager::WordSize()`.
   *
   * MEM means both store (when used as the left child of a MOVE ) and fetch
   * (when used elsewhere).
   * When MEM is used as the left child of a MOVE , it means `store',
   * but anywhere else it means `fetch'.
   *
   * In other languages, we may need to pass another argument `size` to the
   * constructor, since the MEM operation need to know how many bytes of the
   * object to be fetched or stored.
   * However, in Tiger, all the variables and l-values are scalar (P161), which
   * means that they always occupy just one word of memory. So the `size`
   * parameter is omitted, as it's a constant that can be obtained from
   * `frame::RegManager::WordSize()`.
   *
   *
   * @param exp
   */
  explicit MemExp(Exp *exp) : exp_(exp) {}
  ~MemExp() override;

  void Print(FILE *out, int d) const override;
  canon::StmAndExp Canon() override;
  temp::Temp *Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class TempExp : public Exp {
public:
  temp::Temp *temp_;

  /**
   * @brief temporary temp
   *
   * Similar to a register in a real machine.
   * However, the abstract machine can have an infinite number of registers.
   *
   * @param temp
   */
  explicit TempExp(temp::Temp *temp) : temp_(temp) {}
  ~TempExp() override;

  void Print(FILE *out, int d) const override;
  canon::StmAndExp Canon() override;
  temp::Temp *Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class EseqExp : public Exp {
public:
  Stm *stm_;
  Exp *exp_;

  /**
   * @brief Evaluate stm for side effects, and evaluate exp for a result.
   *
   * @param stm
   * @param exp
   */
  EseqExp(Stm *stm, Exp *exp) : stm_(stm), exp_(exp) {}
  ~EseqExp() override;

  void Print(FILE *out, int d) const override;
  canon::StmAndExp Canon() override;
  temp::Temp *Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class NameExp : public Exp {
public:
  temp::Label *name_;

  /**
   * @brief the symbolic constant name
   *
   * Correspond to an assembly language label
   *
   * @param name
   */
  explicit NameExp(temp::Label *name) : name_(name) {}
  ~NameExp() override;

  void Print(FILE *out, int d) const override;
  canon::StmAndExp Canon() override;
  temp::Temp *Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class ConstExp : public Exp {
public:
  int consti_;

  /**
   * @brief the integer constant i
   *
   * @param consti
   */
  explicit ConstExp(int consti) : consti_(consti) {}
  ~ConstExp() override;

  void Print(FILE *out, int d) const override;
  canon::StmAndExp Canon() override;
  temp::Temp *Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class CallExp : public Exp {
public:
  Exp *fun_;
  ExpList *args_;

  /**
   * @brief A procedure call.
   *
   * Evaluation order: subexpression fun, argument list l, from left to right.
   *
   * @param fun
   * @param args
   */
  CallExp(Exp *fun, ExpList *args) : fun_(fun), args_(args) {}
  ~CallExp() override;

  void Print(FILE *out, int d) const override;
  canon::StmAndExp Canon() override;
  temp::Temp *Munch(assem::InstrList &instr_list, std::string_view fs) override;
};

class ExpList {
public:
  ExpList() = default;
  ExpList(std::initializer_list<Exp *> list) : exp_list_(list) {}

  void Append(Exp *exp) { exp_list_.push_back(exp); }
  void Insert(Exp *exp) { exp_list_.push_front(exp); }
  std::list<Exp *> &GetNonConstList() { return exp_list_; }
  const std::list<Exp *> &GetList() { return exp_list_; }
  temp::TempList *MunchArgs(assem::InstrList &instr_list, std::string_view fs);

private:
  std::list<Exp *> exp_list_;
};

class StmList {
  friend class canon::Canon;

public:
  StmList() = default;

  const std::list<Stm *> &GetList() { return stm_list_; }
  void Linear(Stm *stm);
  void Print(FILE *out) const;

private:
  std::list<Stm *> stm_list_;
};

RelOp NotRel(RelOp);  // a op b == not(a NotRel(op) b)
RelOp Commute(RelOp); // a op b == b Commute(op) a

} // namespace tree

#endif // TIGER_TRANSLATE_TREE_H_
