#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"
#include <functional>
#include <sstream>

#define NOT_EXIST_NODE                                                         \
  throw std::runtime_error("the given node doesn't exist in the graph")

#ifndef NDEBUG
#define REMOVE_WITH_CHECK(set, node)                                           \
  {                                                                            \
    if (!set.erase(node))                                                      \
      throw std::runtime_error("the given node doesn't exist in the set");     \
  }
#define INSERT_WITH_CHECK(set, node)                                           \
  {                                                                            \
    if (set.count(node))                                                       \
      throw std::runtime_error("the given node already exists in the set");    \
    set.insert(node);                                                          \
  }
#else
#define REMOVE_WITH_CHECK(set, node)                                           \
  { set.erase(node); }
#define INSERT_WITH_CHECK(set, node)                                           \
  { set.insert(node); }
#endif

extern frame::RegManager *reg_manager;

namespace assem {

void MoveInstr::replace_use(temp::Temp *from, temp::Temp *to) {
  for (temp::Temp *&temp : src_->GetList()) {
    if (temp == from) {
      temp = to;
    }
  }
}
void MoveInstr::replace_def(temp::Temp *from, temp::Temp *to) {
  for (temp::Temp *&temp : dst_->GetList()) {
    if (temp == from) {
      temp = to;
    }
  }
}
void OperInstr::replace_use(temp::Temp *from, temp::Temp *to) {
  for (temp::Temp *&temp : src_->GetList()) {
    if (temp == from) {
      temp = to;
    }
  }
}
void OperInstr::replace_def(temp::Temp *from, temp::Temp *to) {
  for (temp::Temp *&temp : dst_->GetList()) {
    if (temp == from) {
      temp = to;
    }
  }
}
} // namespace assem

namespace ra {

TempNodeSet from_temp_list(temp::TempList const *temp_list) {
  TempNodeSet res{};
  for (const auto temp : temp_list->GetList()) {
    res.insert(temp);
  }
  return res;
}

std::size_t
InterfereGraph::hash_temp_pair_(const BitMap_Underlying &temp_pair) {
  auto [u, v] = temp_pair;
  std::stringstream ss{};
  ss << u->Int() << v->Int();
  return std::hash<std::string>{}(ss.str());
}

[[deprecated]] void InterfereGraph::add_node(TempNode temp,
                                             bool is_precolored) {
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
  if (!is_precolored(u)) {
    adjList_[u].insert(v);
    degree_[u]++;
  }
  if (!is_precolored(v)) {
    adjList_[v].insert(u);
    degree_[v]++;
  }
}

std::size_t InterfereGraph::degree(TempNode v) const {
  if (is_precolored(v) || degree_.count(v) == 0) {
    return std::numeric_limits<std::size_t>::max();
  }
  return degree_.at(v);
}

auto InterfereGraph::adj_of(TempNode v) const -> const TempNodeSet & {
  // Fallback for an empty adj set.
  static TempNodeSet empty_set{};
  if (adjList_.count(v) == 0) {
    return empty_set;
  }
  return adjList_.at(v);
}

bool InterfereGraph::adj_set_contain(TempNode u, TempNode v) const {
  return adjSet_.count({u, v}) == 1;
}

void InterfereGraph::decrease_degree(TempNode v) {
  assert(degree_.count(v));
  degree_[v]--;
}

bool is_move_instr(assem::Instr *instr) {
  return typeid(*instr) == typeid(assem::MoveInstr);
}

[[deprecated]] std::vector<std::tuple<TempNode, bool>>
RegAllocator::extract_temps(assem::Instr *assem) {
  std::vector<std::tuple<TempNode, bool>> res{};

  auto move_instr = static_cast<assem::MoveInstr *>(assem);
  auto dst = move_instr->Def()->GetList();
  auto src = move_instr->Use()->GetList();
  for (const auto temp : dst) {
    res.emplace_back(temp, is_precolored(temp));
  }
  for (const auto temp : src) {
    res.emplace_back(temp, is_precolored(temp));
  }
  return res;
}

bool is_precolored(TempNode n) {
  return reg_manager->temp_map_->Look(const_cast<temp::Temp *>(n)) != nullptr;
}

std::pair<TempNode, TempNode>
RegAllocator::translate_move_instr(assem::MoveInstr *instr) {
  TempNode n1 = instr->src_->GetList().front();
  TempNode n2 = instr->dst_->GetList().front();
  return {n1, n2};
}

TempNode RegAllocator::heuristic_select_spill() const {
  return *std::max_element(begin(spill_worklist), end(spill_worklist),
                           [this](TempNode n1, TempNode n2) -> bool {
                             return interfere_graph_.degree(n1) <
                                    interfere_graph_.degree(n2);
                           });
}

void RegAllocator::RegAlloc() {
  livenessAnalysis();
  build_interfere_graph();
  make_worklist();
  while (true) {
    if (!simplify_worklist.empty()) {
      simplify();
    } else if (!worklist_moves.empty()) {
      coalesce();
    } else if (!freeze_worklist.empty()) {
      freeze();
    } else if (!spill_worklist.empty()) {
      select_spill();
    } else {
      break;
    }
  }
  assign_colors();
  if (!spilled_nodes.empty()) {
    rewrite_program();
    RegAlloc();
  } else {
    // End recursive
    remove_redundant_moves();
    make_result();
  }
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

  for (auto block : node_list) {
    temp::TempList live(
        *liveness_->out(block)); // copied because it can be modified.
    const auto instr = block->NodeInfo();

    if (is_move_instr(instr)) {
      auto move_instr = static_cast<assem::MoveInstr *>(instr);
      assert(move_instr->Def()->GetList().size() == 1);
      assert(move_instr->Use()->GetList().size() == 1);

      live = temp_diff(live, *move_instr->Use());

      {
        auto def_and_use = temp_union(*move_instr->Def(), *move_instr->Use());
        for (const auto temp : def_and_use.GetList()) {
          // Automatically insert the set of instructions if it doesn't exist.
          move_list[temp].insert(move_instr);
        }
      }
      worklist_moves.insert(move_instr);
    }
    // Add interference edges.
    {
      temp::TempList instr_def(*instr->Def());
      instr_def.sort();
      live = temp_union(live, instr_def);
    }
    for (const auto temp_def : instr->Def()->GetList()) {
      for (const auto temp_out : live.GetList()) {
        this->interfere_graph_.add_edge(temp_def, temp_out);
      }
    }
  }
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
  if (move_list.count(n) == 0) {
    return {};
  }

  // tmp := union active_moves and worklist_moves
  MoveInstrSet tmp = set_union(active_moves, worklist_moves);

  const auto &moves = move_list.at(n);

  // intersect move_list[n] and tmp
  auto res = set_intersect(moves, tmp);
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

void RegAllocator::simplify() {
  auto n = *begin(simplify_worklist);
  simplify_worklist.erase(n);
  selected_stack.push(n);
  for (const auto m : adjacent(n)) {
    decrement_degree(m);
  }
}
void RegAllocator::decrement_degree(TempNode m) {
  if (is_precolored(m)) {
    return;
  }
  auto d = interfere_graph_.degree(m);
  interfere_graph_.decrease_degree(m);
  if (d == KColors) {
    {
      auto to_enable_move = adjacent(m);
      to_enable_move.insert(m);
      enable_moves(to_enable_move);
    }
    spill_worklist.erase(m);
    if (move_related(m)) {
      freeze_worklist.insert(m);
    } else {
      simplify_worklist.insert(m);
    }
  }
}

void RegAllocator::enable_moves(const TempNodeSet &nodes) {
  for (const auto n : nodes) {
    for (const auto m : node_moves(n)) {
      if (active_moves.count(m)) {
        active_moves.erase(m);
        INSERT_WITH_CHECK(worklist_moves, m);
      }
    }
  }
}

TempNode RegAllocator::get_alias(TempNode n) const {
  if (coalesced_nodes.count(n)) {
    return get_alias(alias.at(n));
  } else {
    return n;
  }
}

void RegAllocator::add_worklist(TempNode u) {
  if (precolored.count(u) == 0 && !move_related(u) &&
      interfere_graph_.degree(u) < KColors) {
    freeze_worklist.erase(u);
    simplify_worklist.insert(u);
  }
}

bool RegAllocator::OK(TempNode t, TempNode r) const {
  return interfere_graph_.degree(t) < KColors || precolored.count(t) ||
         interfere_graph_.adj_set_contain(t, r);
}

bool RegAllocator::Briggs(TempNode u, TempNode v) const {
  return Conservative(
      set_union(interfere_graph_.adj_of(u), interfere_graph_.adj_of(v)));
}

bool RegAllocator::Conservative(const TempNodeSet &nodes) const {
  return std::count_if(begin(nodes), end(nodes), [this](TempNode n) -> bool {
           return interfere_graph_.degree(n) >= KColors;
         }) < KColors;
}

void RegAllocator::combine(TempNode u, TempNode v) {
  if (freeze_worklist.count(v)) {
    REMOVE_WITH_CHECK(freeze_worklist, v);
  } else {
    spill_worklist.erase(v);
  }
  coalesced_nodes.insert(v);
  alias[v] = u;
  move_list[u] = set_union(move_list.at(u), move_list.at(v));

  for (const auto t : interfere_graph_.adj_of(v)) {
    interfere_graph_.add_edge(t, u);
    decrement_degree(t);
  }

  if (interfere_graph_.degree(u) >= KColors && freeze_worklist.count(u)) {
    freeze_worklist.erase(u);
    spill_worklist.insert(u);
  }
}

void RegAllocator::coalesce() {
  auto instr = *begin(worklist_moves);
  auto [x, y] = translate_move_instr(instr);
  auto u = get_alias(x);
  auto v = get_alias(y);
  if (precolored.count(v)) {
    std::swap(u, v);
  }
  worklist_moves.erase(begin(worklist_moves));

  if (u == v) {
    coalesced_moves.insert(instr);
    add_worklist(u);
  } else if (precolored.count(v) /* both are precolored */ ||
             interfere_graph_.adj_set_contain(u, v) /* interfered MOVE */) {
    constrained_moves.insert(instr);
    add_worklist(u);
    add_worklist(v);
  } else if (precolored.count(u) &&
                 std::all_of(
                     begin(interfere_graph_.adj_of(v)),
                     end(interfere_graph_.adj_of(v)),
                     [this, u](TempNode t) -> bool { return OK(t, u); }) ||
             !precolored.count(u) && Briggs(u, v)) {
    coalesced_moves.insert(instr);
    combine(u, v);
    add_worklist(u);
  } else {
    active_moves.insert(instr);
  }
}

void RegAllocator::freeze() {
  auto u = *begin(freeze_worklist);
  freeze_worklist.erase(begin(freeze_worklist));
  simplify_worklist.insert(u);
  freeze_moves(u);
}

void RegAllocator::freeze_moves(TempNode u) {
  for (const auto m : node_moves(u)) {
    TempNode v{};
    auto [x, y] = translate_move_instr(m);
    if (get_alias(x) == get_alias(y)) {
      v = get_alias(x);
    } else {
      v = get_alias(y);
    }
    REMOVE_WITH_CHECK(active_moves, m);
    INSERT_WITH_CHECK(frozen_moves, m);
  }
}

void RegAllocator::select_spill() {
  auto m = heuristic_select_spill();
  REMOVE_WITH_CHECK(spill_worklist, m);
  INSERT_WITH_CHECK(simplify_worklist, m);
  freeze_moves(m);
}

void RegAllocator::assign_colors() {
  while (!selected_stack.empty()) {
    auto n = selected_stack.pop();
    auto ok_colors = retrieve_general_registers();
    for (const auto w : interfere_graph_.adj_of(n)) {
      auto alias_w = get_alias(w);
      if (colored_nodes.count(alias_w) || precolored.count(alias_w)) {
        ok_colors.erase(color.at(alias_w));
      }
    }
    if (ok_colors.empty()) {
      spilled_nodes.insert(n);
    } else {
      colored_nodes.insert(n);
      color[n] = *begin(ok_colors);
    }
  }
  for (const auto n : coalesced_nodes) {
    color[n] = color.at(get_alias(n));
  }
}

void RegAllocator::rewrite_program() {
  // Map the spilled temps to their new memory locations.
  TempNodeMap<frame::Access *> access_map{};
  // Allocate new memory locations for spilled temps.
  for (const auto n : spilled_nodes) {
    auto access = frame_->allocateLocal(true);
    access_map[n] = access;
  }

  TempNodeSet new_temps{};
  std::list<assem::Instr *> &instr_list =
      assem_instr_->GetInstrList()->GetList();

  // Rewrite
  for (auto it = begin(instr_list); it != end(instr_list); ++it) {
    auto instr = *it;
    int insert_before = 0;
    int insert_next = 0;
    // Check use
    {
      TempNodeSet use_set = from_temp_list(instr->Use());
      auto conflict_uses = set_intersect(use_set, spilled_nodes);
      for (const auto temp : conflict_uses) {
        auto r = temp::TempFactory::NewTemp();
        new_temps.insert(r);
        instr->replace_use(const_cast<temp::Temp *>(temp), r);

        {
          frame::Immediate offset = drag_offset(access_map.at(temp));
          std::stringstream ss{};
          ss << "movq ";
          if (offset != 0) {
            ss << offset;
          }
          // Fetch the value out from memory before each use.
          ss << "(`s0), `d0";
          auto new_instr = new assem::OperInstr(
              ss.str(), new temp::TempList{r},
              new temp::TempList{reg_manager->FramePointer()}, nullptr);
          instr_list.insert(it, new_instr);
          insert_before++;
        }
      }
    }

    // Check def.
    {
      TempNodeSet def_set = from_temp_list(instr->Def());
      auto conflict_defs = set_intersect(def_set, spilled_nodes);
      for (const auto temp : conflict_defs) {
        auto r = temp::TempFactory::NewTemp();
        new_temps.insert(r);
        instr->replace_def(const_cast<temp::Temp *>(temp), r);

        {
          frame::Immediate offset = drag_offset(access_map.at(temp));
          std::stringstream ss{};
          ss << "movq ";
          // Store the value into memory after each def.
          ss << "`s0, ";
          if (offset != 0) {
            ss << offset;
          }
          ss << "(`s1)";
          auto new_instr = new assem::OperInstr(
              ss.str(), nullptr,
              new temp::TempList{r, reg_manager->FramePointer()}, nullptr);
          instr_list.insert(next(it), new_instr);
          insert_next++;
        }
      }
    }
    // Move the iterator, in case of infinite loop.
    advance(it, insert_next);
  }

  spilled_nodes.clear();
  initial = set_union(colored_nodes, coalesced_nodes);
  initial = set_union(initial, new_temps);
  colored_nodes.clear();
  coalesced_nodes.clear();
}

void RegAllocator::make_result() {
  temp::Map *colored = temp::Map::Empty();
  for (auto [temp, reg] : color) {
    auto target_color =
        reg_manager->temp_map_->Look(const_cast<temp::Temp *>(reg));

    colored->Enter(const_cast<temp::Temp *>(temp), target_color);
  }
  result_.reset(new Result(colored, assem_instr_->GetInstrList()));
}

auto RegAllocator::retrieve_general_registers() -> TempNodeSet {
  return from_temp_list(reg_manager->Registers());
}

frame::Immediate RegAllocator::drag_offset(frame::Access *access) {
  assert(access->get_offset().has_value());
  return access->get_offset().value();
}

RegAllocator::RegAllocator(frame::Frame *frame,
                           std::unique_ptr<cg::AssemInstr> assem_instr)
    : assem_instr_{std::move(assem_instr)}, frame_{frame}, result_{nullptr} {

  std::list<assem::Instr *> &instr_list =
      assem_instr_->GetInstrList()->GetList();

  for (const auto instr : instr_list) {
    temp::TempList def(*instr->Def());
    temp::TempList use(*instr->Use());
    def.sort();
    use.sort();
    auto all_temps = temp::temp_union(def, use);
    for (const auto temp : all_temps.GetList()) {
      if (precolored.count(temp) || initial.count(temp)) {
        continue;
      }
      if (is_precolored(temp)) {
        INSERT_WITH_CHECK(precolored, temp);
        color[temp] = temp;
      } else {
        INSERT_WITH_CHECK(initial, temp);
      }
    }
  }
}

void RegAllocator::remove_redundant_moves() {
  std::list<assem::Instr *> &instr_list =
      assem_instr_->GetInstrList()->GetList();
  for (auto it = begin(instr_list); it != end(instr_list); ++it) {
    auto instr = *it;
    if (!is_move_instr(instr)) {
      continue;
    }
    auto [x, y] = translate_move_instr(static_cast<assem::MoveInstr *>(instr));
    auto u = get_alias(x);
    auto v = get_alias(y);
    if (u == v) {
      it = instr_list.erase(it);
      it--;
    }
  }
}

} // namespace ra