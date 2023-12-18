#include "tiger/liveness/flowgraph.h"

namespace fg {

template <typename T> T *unwrap_instr(assem::Instr *instr) {
  static_assert(std::is_base_of_v<assem::Instr, T>,
                "T must be a derived type of assem::Instr");
  if (typeid(*instr) != typeid(T)) {
    return nullptr;
  }
  return static_cast<T *>(instr);
}

void FlowGraphFactory::AssemFlowGraph() {
  auto instr_list = this->instr_list_->GetList();
  // Step 1: enter all labels.
  for (auto instr : instr_list) {
    if (auto label_instr = unwrap_instr<assem::LabelInstr>(instr);
        label_instr) {
      auto fnode = this->flowgraph_->NewNode(instr);
      this->label_map_->Enter(label_instr->label_, fnode);
    }
  }
  FNode *prev_node = nullptr;
  for (auto instr : instr_list) {

    FNode *fnode{};
    if (auto label_instr = unwrap_instr<assem::LabelInstr>(instr);
        label_instr) {
      fnode = this->label_map_->Look(label_instr->label_);
    } else {
      fnode = this->flowgraph_->NewNode(instr);
    }
    assert(fnode);

    if (prev_node) {
      this->flowgraph_->AddEdge(prev_node, fnode);
    }
    prev_node = fnode;

    if (auto oper_instr = unwrap_instr<assem::OperInstr>(instr); oper_instr) {
      // If there are jumps...
      if (oper_instr->jumps_ != nullptr) {
        for (auto target_label : *oper_instr->jumps_->labels_) {
          auto target_fnode = this->label_map_->Look(target_label);
          assert(target_fnode);
          this->flowgraph_->AddEdge(fnode, target_fnode);
        }
      }
    }
  }
}

} // namespace fg

namespace assem {

temp::TempList const *enable_non_null(temp::TempList const *temp_list) {
  return temp_list ? temp_list : new temp::TempList{};
}

temp::TempList const *LabelInstr::Def() const { return new temp::TempList{}; }

temp::TempList const *LabelInstr::Use() const { return new temp::TempList{}; }

temp::TempList const *MoveInstr::Def() const {
  return enable_non_null(this->dst_);
}

temp::TempList const *OperInstr::Def() const {
  return enable_non_null(this->dst_);
}

temp::TempList const *MoveInstr::Use() const {
  return enable_non_null(this->src_);
}

temp::TempList const *OperInstr::Use() const {
  return enable_non_null(this->src_);
}

// namespace assem
} // namespace assem