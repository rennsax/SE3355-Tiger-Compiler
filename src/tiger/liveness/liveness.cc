#include "tiger/liveness/liveness.h"

extern frame::RegManager *reg_manager;

namespace live {

bool MoveList::Contain(INodePtr src, INodePtr dst) {
  return std::any_of(move_list_.cbegin(), move_list_.cend(),
                     [src, dst](std::pair<INodePtr, INodePtr> move) {
                       return move.first == src && move.second == dst;
                     });
}

void MoveList::Delete(INodePtr src, INodePtr dst) {
  assert(src && dst);
  auto move_it = move_list_.begin();
  for (; move_it != move_list_.end(); move_it++) {
    if (move_it->first == src && move_it->second == dst) {
      break;
    }
  }
  move_list_.erase(move_it);
}

MoveList *MoveList::Union(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : move_list_) {
    res->move_list_.push_back(move);
  }
  for (auto move : list->GetList()) {
    if (!res->Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

MoveList *MoveList::Intersect(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : list->GetList()) {
    if (Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

void LiveGraphFactory::LiveMap() {
  auto node_list = this->flowgraph_->Nodes()->GetList();

  // Initialize
  for (auto node : node_list) {
    this->in_->Enter(node, new temp::TempList{});
    this->out_->Enter(node, new temp::TempList{});
  }

  bool has_change = false;
  do {
    has_change = false;
    // Reverse
    for (auto it = rbegin(node_list); it != rend(node_list); ++it) {
      auto node = *it;
      auto instr = node->NodeInfo();
      auto cur_in = this->in_->Look(node);
      auto cur_out = this->out_->Look(node);
      assert(cur_in && cur_out);

      // out[n] = U in[s] for s in succ[n]
      {
        temp::TempList dest{};

        for (auto s : node->Succ()->GetList()) {
          auto s_out = this->get_in_(s);
          dest = temp_union(dest, *s_out);
        }

        if (dest.GetList().size() > cur_out->GetList().size()) {
          has_change = true;
          cur_out->swap(dest);
        }
      }

      // in[n] = use[n] U (out[n] \ def[n])
      {
        temp::TempList kill(*instr->Def());
        kill.sort();
        auto diff_set = temp_diff(*cur_out, kill);

        temp::TempList gen(*instr->Use());
        gen.sort();
        auto dest = temp_union(gen, diff_set);

        assert(dest.GetList().size() >= cur_in->GetList().size());
        if (dest.GetList().size() > cur_in->GetList().size()) {
          has_change = true;
          cur_in->swap(dest);
        }
      }
    }

  } while (has_change);
}

void LiveGraphFactory::InterfGraph() { assert(0); }

temp::TempList *
LiveGraphFactory::get_out_(graph::Node<assem::Instr> *node) const noexcept {
  auto res = this->out_->Look(node);
  assert(res);
  return res;
}

temp::TempList *
LiveGraphFactory::get_in_(graph::Node<assem::Instr> *node) const noexcept {
  auto res = this->in_->Look(node);
  assert(res);
  return res;
}

void LiveGraphFactory::Liveness() { LiveMap(); }

} // namespace live
