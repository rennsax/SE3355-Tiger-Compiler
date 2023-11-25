#include "tiger/frame/x64frame.h"

// TODO what's the global variable?
extern frame::RegManager *reg_manager;

namespace frame {
class InFrameAccess : public Access {
public:
  int offset;

  explicit InFrameAccess(int offset) : offset(offset) {}

  tree::Exp *toExp(tree::Exp *framePtr) const override {
    // Simple variable. The access depends on the frame pointer.
    return new tree::MemExp(new tree::BinopExp(tree::BinOp::PLUS_OP, framePtr,
                                               new tree::ConstExp(offset)));
  }
};

class InRegAccess : public Access {
public:
  temp::Temp *reg;

  explicit InRegAccess(temp::Temp *reg) : reg(reg) {}
  tree::Exp *toExp(tree::Exp *framePtr) const override {
    // A variable in the register.
    // Independent with the frame pointer, so unused.
    return new tree::TempExp(reg);
  }
};

/* TODO: Put your lab5 code here */

} // namespace frame
