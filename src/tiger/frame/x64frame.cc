#include "tiger/frame/x64frame.h"

extern frame::RegManager *reg_manager;

namespace frame {

X64RegManager::X64RegManager() {
  for (int i = 0; i < static_cast<int>(Register::COUNT); ++i) {
    auto temp = temp::TempFactory::NewTemp();
    this->regs_.push_back(temp);
    std::string name{KRegisterNames.at(i)};
    this->temp_map_->Enter(temp, &name);
  }
}

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
    delete framePtr;
    return new tree::TempExp(reg);
  }
};

frame::Frame *newFrame(temp::Label *f, const std::list<bool> &formals) {
  auto x64_frame = new X64Frame(f, formals);
  return x64_frame;
}

X64Frame::X64Frame(temp::Label *label, const std::list<bool> &formals) {
  this->name_ = label;
  for (auto escape : formals) {
    auto formal = this->allocateLocal(escape);
    this->formals_.push_back(formal);
  }
}

frame::Access *X64Frame::allocateLocal(bool escape) {
  if (!escape) {
    auto temp = temp::TempFactory::NewTemp();
    return new frame::InRegAccess(temp);
  }
  int offset = -KX64WordSize * this->locals_.size();
  auto access = new frame::InFrameAccess(offset);
  this->locals_.push_back(access);
  return access;
}

} // namespace frame
