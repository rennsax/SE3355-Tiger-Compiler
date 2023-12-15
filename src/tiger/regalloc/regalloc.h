#ifndef TIGER_REGALLOC_REGALLOC_H_
#define TIGER_REGALLOC_REGALLOC_H_

#include "tiger/codegen/assem.h"
#include "tiger/codegen/codegen.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/regalloc/color.h"
#include "tiger/util/graph.h"
#include <map>
#include <set>
#include <stack>
#include <unordered_set>

namespace ra {

class Result {
public:
  temp::Map *coloring_;
  assem::InstrList *il_;

  Result() : coloring_(nullptr), il_(nullptr) {}
  Result(temp::Map *coloring, assem::InstrList *il)
      : coloring_(coloring), il_(il) {}
  Result(const Result &result) = delete;
  Result(Result &&result) = delete;
  Result &operator=(const Result &result) = delete;
  Result &operator=(Result &&result) = delete;
  ~Result();
};

using TempNode = temp::Temp const *;

struct TempCmp {
  bool operator()(TempNode lhs, TempNode rhs) const {
    return lhs->Int() < rhs->Int();
  }
};

struct TempEqualTo {
  bool operator()(TempNode lhs, TempNode rhs) const {
    return lhs->Int() == rhs->Int();
  }
};

struct TempHash {
  std::size_t operator()(TempNode temp) const { return temp->Int() - 100; }
};

using TempNodeSet = std::set<TempNode, TempCmp>;
template <typename T> using TempNodeMap = std::map<TempNode, T, TempCmp>;

template <typename T, typename Cmp>
std::set<T, Cmp> set_union(const std::set<T, Cmp> &lhs,
                           const std::set<T, Cmp> &rhs) {
  std::set<T, Cmp> res{0, lhs.key_comp()};
  std::set_union(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                 std::inserter(res, res.begin()));
  return res;
}

template <typename T, typename Cmp>
std::set<T, Cmp> set_diff(const std::set<T, Cmp> &lhs,
                          const std::set<T, Cmp> &rhs) {
  std::set<T, Cmp> res{0, lhs.key_comp()};
  std::set_difference(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                      std::inserter(res, res.begin()));
  return res;
}

template <typename T, typename Cmp>
std::set<T, Cmp> set_intersect(const std::set<T, Cmp> &lhs,
                               const std::set<T, Cmp> &rhs) {
  std::set<T, Cmp> res{0, lhs.key_comp()};
  std::set_intersection(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                        std::inserter(res, res.begin()));
  return res;
}

class InterfereGraph {
public:
  InterfereGraph() = default;

  void add_node(TempNode temp, bool is_precolored = false);
  void add_edge(TempNode u, TempNode v);
  std::size_t degree(TempNode v) const;
  const TempNodeSet &adj_of(TempNode v) const;

  TempNodeSet &get_precolored() & { return precolored; }
  TempNodeSet &get_initial() & { return initial; }

private:
  using BitMap_Underlying = std::pair<TempNode, TempNode>;

  static std::size_t hash_temp_pair_(const BitMap_Underlying &temp_pair);

  /**
   * @brief The set of interference edges (u, v) in the graph.
   * Tell if u and v are adjacent quickly.
   *
   * @note (u, v) in adjSet <=> (v, u) in adjSet.
   */
  std::unordered_set<BitMap_Underlying,
                     decltype(&InterfereGraph::hash_temp_pair_)>
      adjSet_{0, &InterfereGraph::hash_temp_pair_};
  /**
   * @brief Adjacency list representation of the graph.
   * Get all the nodes adjacent to node X.
   *
   */
  TempNodeMap<TempNodeSet> adjList_{};

  /**
   * @brief Current degree of each node.
   *
   * @note degree[n] == adjList[n].size isn't satisfied all the time.
   *
   */
  TempNodeMap<std::size_t> degree_{};

  /// Machine registers, preassigned a color.
  TempNodeSet precolored{};
  /// Temporary registers, neither precolored nor processed.
  TempNodeSet initial{};
};

class RegAllocator {
  struct NodeAdditionalInfo {
    // TODO
  };

public:
  static constexpr int KColors = static_cast<int>(frame::Register::COUNT);

  RegAllocator(frame::Frame *frame, std::unique_ptr<cg::AssemInstr> assem_instr)
      : assem_instr_{std::move(assem_instr)}, frame_{frame}, result_{nullptr} {
    // TODO
  }

  void RegAlloc();

  std::unique_ptr<ra::Result> TransferResult() { return std::move(result_); }

private:
  using MoveInstr = assem::MoveInstr *;
  using MoveInstrSet = std::set<MoveInstr>;

  // TODO

  // Input.
  frame::Frame *frame_{};
  std::unique_ptr<cg::AssemInstr> assem_instr_{};

  // Liveness products.
  std::unique_ptr<fg::FGraph> flow_graph_{};
  std::unique_ptr<live::LivenessResult> liveness_{};

  /**
   * @brief Node work-lists, sets, and stacks.
   * The following lists and sets are always mutually disjoint and every node is
   * always in exactly one of the sets or lists
   *
   */

  /// Machine registers, preassigned a color.
  TempNodeSet precolored{};
  /// Temporary registers, neither precolored nor processed.
  TempNodeSet initial{};
  /// low-degree non-move-related nodes.
  TempNodeSet simplify_worklist{};
  /// low-degree move-related nodes.
  TempNodeSet freeze_worklist{};
  /// high-degree nodes.
  TempNodeSet spill_worklist{};
  TempNodeSet spilled_nodes{};
  TempNodeSet coalesced_nodes{};
  TempNodeSet colored_nodes{};
  struct {
    const TempNodeSet &get_set() const & { return set_; }
    void push(TempNode n) {
      set_.insert(n);
      stack_.push(n);
    }
    TempNode pop() {
      auto n = stack_.top();
      stack_.pop();
      set_.erase(n);
      return n;
    }

  private:
    TempNodeSet set_{};
    std::stack<TempNode> stack_{};
  } selected_stack{};
  // TempNodeSet select_stack{};

  /**
   * @brief Move sets.
   * There are five sets of move instructions, and every move is in exactly one
   * of these sets (after Build through the end of Main)
   */

  /// Moves that have been coalesced.
  MoveInstrSet coalesced_moves{};
  /// Moves whose source and target interfere.
  MoveInstrSet constrained_moves{};
  /// Moves that will no longer be considered for coalescing.
  MoveInstrSet frozen_moves{};
  /// Moves enabled for possible coalescing.
  MoveInstrSet worklist_moves{};
  /// Moves not yet ready for coalescing.
  MoveInstrSet active_moves{};

  /**
   * @brief Other data structures.
   *
   */

  /// adjSet, adjList, degree
  InterfereGraph interfere_graph_{};

  /// A mapping from a node to the list of moves it is associated with.
  TempNodeMap<MoveInstrSet> move_list{};

  // Final result.
  std::unique_ptr<ra::Result> result_{};

  /**
   * @brief Basic workflow.
   *
   */

  void livenessAnalysis();
  void build_interfere_graph();
  void make_worklist();

  /**
   * @brief Helper functions.
   *
   */

  /// Get all valid moves related to n.
  MoveInstrSet node_moves(TempNode n) const;
  bool move_related(TempNode n) const;
  /// Get all actual adjacent node for n.
  TempNodeSet adjacent(TempNode n) const;

  /**
   * @brief Other functions.
   *
   */

  /// Whether the instruction is a move instruction.
  static bool is_move_instr(assem::Instr *);
  /**
   * @brief Extract temps from the instruction.
   *
   * @return std::vector<std::tuple<TempNode, bool>> A list of temporaries. The
   * second value indicates whether the temporary register is precolored.
   */
  static std::vector<std::tuple<TempNode, bool>> extract_temps(assem::Instr *);
};

} // namespace ra

#endif