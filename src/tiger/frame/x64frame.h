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

  temp::Temp *GetRegister(int reg_idx) const override { return regs_[reg_idx]; }

#define REGISTER(sym) (regs_.at(static_cast<std::size_t>(Register::sym)))
  temp::TempList *Registers() override {
    auto res = new temp::TempList{};
    for (int i = 0; i < static_cast<std::size_t>(Register::COUNT); ++i) {
      res->Append(regs_.at(i));
    }
    return res;
  }
  temp::TempList *ArgRegs() override {
    auto res = new temp::TempList{};
    res->Append(REGISTER(RDI));
    res->Append(REGISTER(RSI));
    res->Append(REGISTER(RDX));
    res->Append(REGISTER(RCX));
    res->Append(REGISTER(R8));
    res->Append(REGISTER(R9));
    return res;
  }
  temp::TempList *CallerSaves() override {
    auto res = new temp::TempList{};
    res->Append(REGISTER(RAX));
    res->Append(REGISTER(RDI));
    res->Append(REGISTER(RSI));
    res->Append(REGISTER(RDX));
    res->Append(REGISTER(RCX));
    res->Append(REGISTER(R8));
    res->Append(REGISTER(R9));
    res->Append(REGISTER(R10));
    res->Append(REGISTER(R11));
    return res;
  }
  temp::TempList *CalleeSaves() override {
    auto res = new temp::TempList{};
    res->Append(REGISTER(RBP));
    res->Append(REGISTER(RBX));
    res->Append(REGISTER(R12));
    res->Append(REGISTER(R13));
    res->Append(REGISTER(R14));
    res->Append(REGISTER(R15));
    return res;
  }
  temp::TempList *ReturnSink() override {
    // TODO return sink?
    return nullptr;
  }
  temp::Temp *FramePointer() override { return REGISTER(RSP); }
  temp::Temp *StackPointer() override { return REGISTER(RBP); }
  temp::Temp *ReturnValue() override { return REGISTER(RAX); }
#undef REGISTER

private:
  std::vector<temp::Temp *> regs_;
};

class X64Frame : public Frame {
public:
  X64Frame() = delete;

  X64Frame(temp::Label *label, const std::list<bool> &formals);

  frame::Access *allocateLocal(bool escape) override;

  tree::Stm *procEntryExit1(tree::Stm *) const override;

  void procEntryExit2(assem::InstrList &body) const override;

  [[nodiscard]] assem::Proc *procEntryExit3(assem::InstrList *) const override;

  virtual std::string GetLabel() const override { return this->name_->Name(); }

  const std::list<frame::Access *> &getFormals() const override {
    return formals_;
  }

private:
  std::list<frame::Access *> formals_;
  std::list<frame::Access *> locals_;
  temp::Label *name_;
};

} // namespace frame
#endif // TIGER_COMPILER_X64FRAME_H
