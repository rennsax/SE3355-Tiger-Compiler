//
// Created by wzl on 2021/10/12.
//

#ifndef TIGER_COMPILER_X64FRAME_H
#define TIGER_COMPILER_X64FRAME_H

#include "tiger/frame/frame.h"

namespace frame {
// x86-64: 64-bit word size, 8 bytes
constexpr int KX64WordSize = 8;

class X64RegManager : public RegManager {
  /* TODO: Put your lab5 code here */
public:
  X64RegManager() = default;

  [[nodiscard]] temp::TempList *Registers() override;
  [[nodiscard]] temp::TempList *ArgRegs() override;
  [[nodiscard]] temp::TempList *CallerSaves() override;
  [[nodiscard]] temp::TempList *CalleeSaves() override;
  [[nodiscard]] temp::TempList *ReturnSink() override;
  [[nodiscard]] temp::Temp *FramePointer() override;
  [[nodiscard]] temp::Temp *StackPointer() override;
  [[nodiscard]] temp::Temp *ReturnValue() override;
};

class X64Frame : public Frame {
  /* TODO: Put your lab5 code here */
};

} // namespace frame
#endif // TIGER_COMPILER_X64FRAME_H
