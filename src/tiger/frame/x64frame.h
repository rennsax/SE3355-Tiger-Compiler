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
    static temp::TempList *general_purpose_regs = nullptr;
    if (!general_purpose_regs) {
      general_purpose_regs = new temp::TempList{};
      for (int i = 0; i < static_cast<std::size_t>(Register::COUNT); ++i) {
        general_purpose_regs->Append(regs_.at(i));
      }
    }
    return general_purpose_regs;
  }
  temp::TempList *ArgRegs() override {
    static temp::TempList *arg_regs = nullptr;
    if (!arg_regs) {
      arg_regs = new temp::TempList{};
      arg_regs->Append(REGISTER(RDI));
      arg_regs->Append(REGISTER(RSI));
      arg_regs->Append(REGISTER(RDX));
      arg_regs->Append(REGISTER(RCX));
      arg_regs->Append(REGISTER(R8));
      arg_regs->Append(REGISTER(R9));
    }
    return arg_regs;
  }
  temp::TempList *CallerSaves() override {
    static temp::TempList *call_clobbered = nullptr;
    if (!call_clobbered) {
      call_clobbered = new temp::TempList{};
      call_clobbered->Append(REGISTER(RAX));
      call_clobbered->Append(REGISTER(RDI));
      call_clobbered->Append(REGISTER(RSI));
      call_clobbered->Append(REGISTER(RDX));
      call_clobbered->Append(REGISTER(RCX));
      call_clobbered->Append(REGISTER(R8));
      call_clobbered->Append(REGISTER(R9));
      call_clobbered->Append(REGISTER(R10));
      call_clobbered->Append(REGISTER(R11));
    }
    return call_clobbered;
  }
  temp::TempList *CalleeSaves() override {
    static temp::TempList *call_preserved = nullptr;
    if (!call_preserved) {
      call_preserved = new temp::TempList{};
      call_preserved->Append(REGISTER(RSP));
      call_preserved->Append(REGISTER(RBP));
      call_preserved->Append(REGISTER(RBX));
      call_preserved->Append(REGISTER(R12));
      call_preserved->Append(REGISTER(R13));
      call_preserved->Append(REGISTER(R14));
      call_preserved->Append(REGISTER(R15));
    }
    return call_preserved;
  }
  temp::TempList *ReturnSink() override {
    static temp::TempList *returnSink = nullptr;
    if (!returnSink) {
      auto callee_savers = this->CalleeSaves();
      returnSink = new temp::TempList(*callee_savers);
    }
    return returnSink;
  }
  temp::Temp *FramePointer() override { return REGISTER(RBP); }
  temp::Temp *StackPointer() override { return REGISTER(RSP); }
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
  // std::list<frame::Access *> locals_;
  temp::Label *name_;
  tree::Stm *view_shift_stm_;
  // Where the new-allocated local goes
  // The number factually equals to the frame size for all locals.
  std::size_t neg_local_offset_;
};

} // namespace frame
#endif // TIGER_COMPILER_X64FRAME_H
