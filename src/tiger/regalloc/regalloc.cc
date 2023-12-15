#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"
#include <functional>
#include <sstream>

#define NOT_EXIST_NODE                                                         \
  throw std::runtime_error("the given node doesn't exist in the graph")

extern frame::RegManager *reg_manager;

namespace ra {

std::size_t
InterfereGraph::hash_temp_pair_(const BitMap_Underlying &temp_pair) {
  auto [u, v] = temp_pair;
  std::stringstream ss{};
  ss << u->Int() << v->Int();
  return std::hash<std::string>{}(ss.str());
}

void InterfereGraph::add_node(TempNode temp, bool is_precolored) {
  if (precolored.count(temp) || initial.count(temp)) {
    return;
  }
  if (is_precolored) {
    precolored.insert(temp);
  } else {
    initial.insert(temp);
    // FIXME necessary?
    adjList_.emplace(std::make_pair(temp, TempNodeSet{}));
    degree_.emplace(std::make_pair(temp, 0));
  }
}

void InterfereGraph::add_edge(TempNode u, TempNode v) {
  if (u != v && adjSet_.count(std::make_pair(u, v))) {
    return;
  }
  adjSet_.insert(std::make_pair(u, v));
  adjSet_.insert(std::make_pair(v, u));
  if (!precolored.count(u)) {
    adjList_[u].insert(v);
    degree_[u]++;
  }
  if (!precolored.count(v)) {
    adjList_[v].insert(u);
    degree_[v]++;
  }
}

std::size_t InterfereGraph::degree(TempNode v) const { return degree_.at(v); }

auto InterfereGraph::adj_of(TempNode v) const -> const TempNodeSet & {
  return adjList_.at(v);
}

bool RegAllocator::is_move_instr(assem::Instr *instr) {
  return typeid(*instr) == typeid(assem::MoveInstr);
}

std::vector<std::tuple<TempNode, bool>>
RegAllocator::extract_temps(assem::Instr *assem) {
  std::vector<std::tuple<TempNode, bool>> res{};

  auto move_instr = static_cast<assem::MoveInstr *>(assem);
  auto dst = move_instr->Def()->GetList();
  auto src = move_instr->Use()->GetList();
  for (const auto temp : dst) {
    res.emplace_back(temp, reg_manager->temp_map_->Look(temp) != nullptr);
  }
  for (const auto temp : src) {
    res.emplace_back(temp, reg_manager->temp_map_->Look(temp) != nullptr);
  }
  return res;
}

void RegAllocator::RegAlloc() {
  // TODO
  livenessAnalysis();
  build_interfere_graph();
}

void RegAllocator::livenessAnalysis() {
  fg::FlowGraphFactory fg_factory{assem_instr_->GetInstrList()};
  fg_factory.AssemFlowGraph();
  flow_graph_.reset(fg_factory.GetFlowGraph());

  live::LiveGraphFactory live_factory{flow_graph_.get()};
  live_factory.Liveness();
  liveness_.reset(live_factory.TransferResult());
}

void RegAllocator::build_interfere_graph() {
  // In fact we can merge nodes into some basic blocks, which are nodes of the
  // new graph. With basic blocks liveness analysis can be accelerated.
  // Here we just treat each instruction as a node and a basic block, which
  // makes things simple. See P223.
  auto node_list = this->flow_graph_->Nodes()->GetList();

  // Create all nodes. This step also initialize `initial` and `precolored` in
  // the interfere graph.
  for (const auto node : node_list) {
    auto temps = extract_temps(node->NodeInfo());
    for (const auto temp : temps) {
      std::apply(std::bind(&InterfereGraph::add_node, this->interfere_graph_,
                           std::placeholders::_1, std::placeholders::_2),
                 temp);
    }
  }

  for (auto block : node_list) {
    temp::TempList live(
        *liveness_->out(block)); // copied because it can be modified.
    const auto instr = block->NodeInfo();

    if (is_move_instr(instr)) {
      auto move_instr = static_cast<assem::MoveInstr *>(instr);
      live = temp_diff(live, *move_instr->Use());
      assert(instr->Def()->GetList().size() == 1);
      assert(instr->Use()->GetList().size() == 1);

      {
        auto def_and_use = temp_union(*move_instr->Def(), *move_instr->Use());
        for (const auto temp : def_and_use.GetList()) {
          // Automatically insert the set of instructions if it doesn't exist.
          move_list[temp].insert(move_instr);
        }
      }
      worklist_moves.insert(move_instr);

      // Add interference edges.
      live = temp_union(live, *move_instr->Def());
      for (const auto temp_def : move_instr->Def()->GetList()) {
        for (const auto temp_out : live.GetList()) {
          this->interfere_graph_.add_edge(temp_def, temp_out);
        }
      }
    }
  }

  this->initial = std::move(this->interfere_graph_.get_initial());
  this->precolored = std::move(this->interfere_graph_.get_precolored());
}

void RegAllocator::make_worklist() {
  for (auto temp : initial) {
    if (interfere_graph_.degree(temp) >= KColors) {
      spill_worklist.insert(temp);
    } else if (move_related(temp)) {
      freeze_worklist.insert(temp);
    } else {
      simplify_worklist.insert(temp);
    }
  }
  initial.clear();
}

auto RegAllocator::node_moves(TempNode n) const -> MoveInstrSet {
  // tmp := union active_moves and worklist_moves
  MoveInstrSet tmp = set_union(active_moves, worklist_moves);

  const auto &moves = move_list.at(n);

  // intersect move_list[n] and tmp
  auto res = set_intersect(move_list.at(n), tmp);
  return res;
}

bool RegAllocator::move_related(TempNode n) const {
  return !node_moves(n).empty();
}

TempNodeSet RegAllocator::adjacent(TempNode n) const {
  // tmp := union select_stack coalesced_nodes
  auto tmp = set_union(selected_stack.get_set(), coalesced_nodes);
  auto res = set_diff(interfere_graph_.adj_of(n), tmp);
  return res;
}

} // namespace ra