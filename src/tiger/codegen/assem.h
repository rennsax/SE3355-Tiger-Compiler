#ifndef TIGER_CODEGEN_ASSEM_H_
#define TIGER_CODEGEN_ASSEM_H_

#include <cstdio>
#include <string>
#include <vector>

#include "tiger/frame/temp.h"

namespace assem {

class Targets {
public:
  std::vector<temp::Label *> *labels_;

  explicit Targets(std::vector<temp::Label *> *labels) : labels_(labels) {}
};

/**
 * @brief Assembly language instruction without register assignments.
 *
 */
class Instr {
public:
  virtual ~Instr() = default;

  /**
   * @brief Formats an assembly instruction as a string and prints it to the
   * file.
   *
   * @param out output file.
   * @param m a temp mapping that tells the register assignment (or perhaps just
   * the name) of every temp.
   */
  virtual void Print(FILE *out, temp::Map *m) const = 0;
};

class OperInstr : public Instr {
public:
  /// Assembly-language instruction.
  std::string assem_;
  /// A list of operand registers. May be empty.
  temp::TempList *src_;
  /// A list of result registers. May be empty.
  temp::TempList *dst_;
  /**
   * @brief A list of next instructions.
   *
   * Operations that always fall through to the next instruction have @c
   * jump=NULL; other operations have a list of "target" labels to which they
   * may jump (this list must explicitly include the next instruction if it is
   * possible to fall through to it).
   *
   */
  Targets *jumps_;

  OperInstr(std::string assem, temp::TempList *dst, temp::TempList *src,
            Targets *jumps)
      : assem_(std::move(assem)), dst_(dst), src_(src), jumps_(jumps) {}

  void Print(FILE *out, temp::Map *m) const override;
};

/**
 * @brief A point in a program to which jumps may go.
 *
 */
class LabelInstr : public Instr {
public:
  /// Assembly-language instruction.
  std::string assem_;
  /// @see sym::Symbol#Name() const
  temp::Label *label_;

  LabelInstr(std::string assem, temp::Label *label)
      : assem_(std::move(assem)), label_(label) {}

  void Print(FILE *out, temp::Map *m) const override;
};

/**
 * @brief Like an OperInstr, but can only perform data transfer.
 *
 */
class MoveInstr : public Instr {
public:
  /// Assembly-language instruction.
  std::string assem_;
  /// @sa OperInstr::dst_
  temp::TempList *dst_;
  /// @sa OperInstr::src_
  temp::TempList *src_;

  MoveInstr(std::string assem, temp::TempList *dst, temp::TempList *src)
      : assem_(std::move(assem)), dst_(dst), src_(src) {}

  void Print(FILE *out, temp::Map *m) const override;
};

class InstrList {
public:
  InstrList() = default;

  void Print(FILE *out, temp::Map *m) const;
  void Append(assem::Instr *instr) { instr_list_.push_back(instr); }
  void Remove(assem::Instr *instr) { instr_list_.remove(instr); }
  void Insert(std::list<Instr *>::const_iterator pos, assem::Instr *instr) {
    instr_list_.insert(pos, instr);
  }
  [[nodiscard]] const std::list<Instr *> &GetList() const {
    return instr_list_;
  }

private:
  std::list<Instr *> instr_list_;
};

class Proc {
public:
  std::string prolog_;
  InstrList *body_;
  std::string epilog_;

  Proc(std::string prolog, InstrList *body, std::string epilog)
      : prolog_(std::move(prolog)), body_(body), epilog_(std::move(epilog)) {}
};

} // namespace assem

#endif
