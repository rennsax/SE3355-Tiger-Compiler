//
// Created by wzl on 2021/10/12.
//

#ifndef TIGER_COMPILER_X64FRAME_H
#define TIGER_COMPILER_X64FRAME_H

#include "tiger/frame/frame.h"

namespace frame {
// x86-64: 64-bit word size, 8 bytes
constexpr int KX64WordSize = 8;

enum class Register : int {
  RAX,
  RBX,
  RCX,
  RDX,
  RSI,
  RDI,
  R8,
  R9,
  R10,
  R11,
  R12,
  R13,
  R14,
  R15,
  RBP,
  RSP,
  COUNT,
};

static constexpr std::array<const char *,
                            static_cast<std::size_t>(Register::COUNT)>
    KRegisterNames = {
        "%rax", "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "%r8",  "%r9",
        "%r10", "%r11", "%r12", "%r13", "%r14", "%r15", "%rbp", "%rsp",
};

class X64RegManager : public RegManager {
public:
  X64RegManager();

#define REGISTER(sym) (regs_.at(static_cast<std::size_t>(Register::sym)))
  temp::TempList *Registers() override {}
  temp::TempList *ArgRegs() override {}
  temp::TempList *CallerSaves() override {}
  temp::TempList *CalleeSaves() override {}
  temp::TempList *ReturnSink() override {}
  temp::Temp *FramePointer() override { return REGISTER(RSP); }
  temp::Temp *StackPointer() override {}
  temp::Temp *ReturnValue() override {}
#undef REGISTER
};

class X64Frame : public Frame {
public:
  X64Frame() = delete;

  X64Frame(temp::Label *label, const std::list<bool> &formals);

  frame::Access *allocateLocal(bool escape) override;

  [[nodiscard]] tree::Stm *procEntryExit1(tree::Stm *) const override {}

  [[nodiscard]] assem::InstrList *
  procEntryExit2(assem::InstrList *body) const override {}

  [[nodiscard]] assem::Proc *procEntryExit3(assem::InstrList *) const override {
  }

  virtual std::string GetLabel() const override { return this->name_->Name(); }
};

} // namespace frame
#endif // TIGER_COMPILER_X64FRAME_H
