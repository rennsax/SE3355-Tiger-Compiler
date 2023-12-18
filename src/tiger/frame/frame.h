#ifndef TIGER_FRAME_FRAME_H_
#define TIGER_FRAME_FRAME_H_

#include <list>
#include <memory>
#include <string>
#include <string_view>
#include <optional>

#include "tiger/codegen/assem.h"
#include "tiger/frame/temp.h"
#include "tiger/translate/tree.h"

namespace frame {

/**
 * @brief Handle external calls. For example, call `malloc` implemented by C.
 *
 * (P168) The differences of calling conventions or operating systems causes
 * target-machine-specific details that need to be handled when calling an
 * external function.
 * May have to be adjusted for static links, or underscores in labels, etc.
 *
 * @param callee_label
 * @param args
 * @return tree::Exp*
 */
tree::Exp *externalCall(std::string_view callee_label, tree::ExpList *args);

class RegManager {
public:
  RegManager() : temp_map_(temp::Map::Empty()) {}

  /**
   * @brief Get the reg_idx-th register.
   *
   * @param reg_idx
   * @return temp::Temp*
   */
  virtual temp::Temp *GetRegister(int reg_idx) const = 0;

  /**
   * Get general-purpose registers except RSI
   * NOTE: returned temp list should be in the order of calling convention
   * @return general-purpose registers
   */
  [[nodiscard]] virtual temp::TempList *Registers() = 0;

  /**
   * Get registers which can be used to hold arguments
   * NOTE: returned temp list must be in the order of calling convention
   * @return argument registers
   */
  [[nodiscard]] virtual temp::TempList *ArgRegs() = 0;

  /**
   * Get caller-saved registers
   * NOTE: returned registers must be in the order of calling convention
   * @return caller-saved registers
   */
  [[nodiscard]] virtual temp::TempList *CallerSaves() = 0;

  /**
   * Get callee-saved registers
   * NOTE: returned registers must be in the order of calling convention
   * @return callee-saved registers
   */
  [[nodiscard]] virtual temp::TempList *CalleeSaves() = 0;

  /**
   * Get return-sink registers
   * Used by frame::Frame#procEntryExit2.
   *
   * @return return-sink registers
   */
  [[nodiscard]] virtual temp::TempList *ReturnSink() = 0;

  [[nodiscard]] virtual temp::Temp *FramePointer() = 0;

  [[nodiscard]] virtual temp::Temp *StackPointer() = 0;

  [[nodiscard]] virtual temp::Temp *ReturnValue() = 0;

  temp::Map *temp_map_; // Used in output.cc, cannot be concealed.
};

class Access {
public:
  /**
   * @brief An interface to get the tree expression.
   *
   * The signature corresponds to
   *      T_exp F_Exp(F_access acc, T_exp framePtr);
   * in the textbook P159.
   *
   * This interface mainly handles the scenario when accessing the variable from
   * an inner-nested function, in which case the frame address must be
   * calculated using static link.
   *
   * @param framePtr The frame pointer, passed as tree::TempExp.
   *
   * Why bother to pass the tree expression as an argument?
   * Intuitive solution: Why not let InFrameAccess get the frame pointer by
   * itself?
   * The reason is: the "frame pointer" is not always stored in the register
   * (e.g. %rbp)! And it's caused by the static link. See P159 for more info.
   *
   * @return tree::Exp* mid IR expression
   */
  [[nodiscard]] virtual tree::Exp *toExp(tree::Exp *framePtr) const = 0;
  virtual std::optional<int> get_offset() const = 0;
  virtual ~Access() = default;
};

class Frame {
public:
  /**
   * @brief allocate a local variable (access) for the frame.
   *
   * As each variable declaration is encountered in processing the Tiger
   * program, allocLocal will be called to allocate a temporary or new space in
   * the frame, associated with the name v. (P139)
   *
   * @param escape if false, the access is in register, otherwise it's in the
   * frame memory.
   * @return frame::Access*
   */
  virtual frame::Access *allocateLocal(bool escape) = 0;

  /**
   * @brief Handle view shift and combine the tree statements.
   *
   * The tasks:
   * - Moving incoming formal parameter,
   * - saving and restoring of callee-save register,
   *
   * Implementation (P267):
   * Allocate locals according to the formals when the new frame is created,
   * and generate some IR statements, which are kept by the frame itself.
   * #procEntryExit1 just concatenate them and return it.
   *
   * @return tree::Stm*
   */
  [[nodiscard]] virtual tree::Stm *procEntryExit1(tree::Stm *) const = 0;

  /**
   * @brief Append a "sink" instruction.
   *
   * (P215)
   * This function appends a pseudo "sink" instruction to the function body to
   * tell the register allocator that certain registers are "live" at procedure
   * exit.
   *
   * @param body The function body.
   */
  virtual void procEntryExit2(assem::InstrList &body) const = 0;

  /**
   * @brief Creates the procedure prologue and epilogue assembly language.
   *
   * The prologue includes a definition of the assembly-language constant
   * which denotes the frame size.
   *
   * (P269)
   * First, it calculates the size of the outgoing parameter space in the frame.
   * It's not obvious at this time, so we let #procEntryExit2 to scan the body
   * and record this information.
   * The implementation is time-wasting, but it permits the frame size to grow
   * and shrink even after it is first created.
   *
   * The following instructions should be generated:
   * - Adjust the stack pointer (prologue).
   * - Reset the stack pointer (epilogue).
   * - Return (epilogue).
   *
   * @return assem::Proc*
   */
  [[nodiscard]] virtual assem::Proc *
  procEntryExit3(assem::InstrList *) const = 0;

  virtual std::string GetLabel() const = 0;

  virtual const std::list<frame::Access *> &getFormals() const = 0;
};

/**
 * Fragments
 */

class Frag {
public:
  virtual ~Frag() = default;

  enum OutputPhase {
    Proc,
    String,
  };

  /**
   * Generate assembly for main program
   * @param out FILE object for output assembly file
   */
  virtual void OutputAssem(FILE *out, OutputPhase phase,
                           bool need_ra) const = 0;
};

class StringFrag : public Frag {
public:
  temp::Label *label_;
  std::string str_;

  StringFrag(temp::Label *label, std::string str)
      : label_(label), str_(std::move(str)) {}

  void OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const override;
};

class ProcFrag : public Frag {
public:
  tree::Stm *body_;
  Frame *frame_;

  /**
   * @brief For function definition.
   *
   * @param body the result returned from frame::Frame::procEntryExit1.
   * @param frame the frame descriptor
   */
  ProcFrag(tree::Stm *body, Frame *frame) : body_(body), frame_(frame) {}

  void OutputAssem(FILE *out, OutputPhase phase, bool need_ra) const override;
};

class Frags {
public:
  Frags() = default;
  void PushBack(Frag *frag) { frags_.push_back(frag); }
  const std::list<Frag *> &GetList() { return frags_; }

private:
  std::list<Frag *> frags_;
};

/**
 * @brief Factory to make a new frame for a function f with formal parameters.
 *
 * "View shift" should be considered.
 *
 * @param f the symbol of the function
 * @param formals whether the formals are escaped
 * @return Frame
 */
[[nodiscard]] Frame *newFrame(temp::Label *f, const std::list<bool> &formals);

} // namespace frame

#endif