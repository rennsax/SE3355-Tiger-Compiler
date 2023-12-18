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
#include <queue>
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
  ~Result() {}
};

using TempNode = temp::Temp const *;
struct TempCmp;
using TempNodeSet = std::set<TempNode, TempCmp>;
template <typename T> using TempNodeMap = std::map<TempNode, T, TempCmp>;
using ColorMap = TempNodeMap<TempNode>;

/**
 * @brief Converts a linked list of TempNodes to a TempNodeSet.
 *
 * @param temp_list A pointer to the linked list of TempNodes.
 * @return The TempNodeSet containing the TempNodes from the linked list.
 */
TempNodeSet from_temp_list(temp::TempList const *temp_list);

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

// Set utilities.

template <typename T, typename Cmp>
std::set<T, Cmp> set_union(const std::set<T, Cmp> &lhs,
                           const std::set<T, Cmp> &rhs) {
  std::set<T, Cmp> res{};
  std::set_union(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                 std::inserter(res, res.begin()));
  return res;
}

template <typename T, typename Cmp>
std::set<T, Cmp> set_diff(const std::set<T, Cmp> &lhs,
                          const std::set<T, Cmp> &rhs) {
  std::set<T, Cmp> res{};
  std::set_difference(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                      std::inserter(res, res.begin()));
  return res;
}

template <typename T, typename Cmp>
std::set<T, Cmp> set_intersect(const std::set<T, Cmp> &lhs,
                               const std::set<T, Cmp> &rhs) {
  std::set<T, Cmp> res{};
  std::set_intersection(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                        std::inserter(res, res.begin()));
  return res;
}

/// Whether the register is a machine register ().
bool is_precolored(TempNode n);

/// Whether the instruction is a move instruction.
bool is_move_instr(assem::Instr *);

class InterfereGraph {
  // FIXME merge this class into register allocator
public:
  InterfereGraph(TempNodeSet &precolored, TempNodeSet &initial)
      : precolored{precolored}, initial{initial} {}

  [[deprecated]] void add_node(TempNode temp, bool is_precolored = false);
  void add_edge(TempNode u, TempNode v);
  std::size_t degree(TempNode v) const;
  std::size_t &degree(TempNode v);
  const TempNodeSet &adj_of(TempNode v) const;
  bool adj_set_contain(TempNode u, TempNode v) const;

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
  TempNodeSet &precolored;
  /// Temporary registers, neither precolored nor processed.
  TempNodeSet &initial;
};

class RegAllocator {

public:
  static constexpr int KColors = static_cast<int>(frame::Register::COUNT);

  RegAllocator(frame::Frame *frame,
               std::unique_ptr<cg::AssemInstr> assem_instr);

  void RegAlloc();

  std::unique_ptr<ra::Result> TransferResult() { return std::move(result_); }

private:
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
  /// Initialized at the very beginning. @sa #ReAllocator
  TempNodeSet precolored{};
  /// Temporary registers, neither precolored nor processed.
  /// Initialized at the very beginning. @sa #ReAllocator
  TempNodeSet initial{};
  /// low-degree non-move-related nodes.
  TempNodeSet simplify_worklist{};
  /// low-degree move-related nodes.
  TempNodeSet freeze_worklist{};
  /// high-degree nodes.
  TempNodeSet spill_worklist{};
  TempNodeSet spilled_nodes{};
  TempNodeSet coalesced_nodes{};
  /// Nodes successfully colored.
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
    bool empty() const {
      assert(set_.empty() == stack_.empty());
      return set_.empty();
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

  using MoveInstr = assem::MoveInstr *;
  using MoveInstrSet = std::set<MoveInstr>;

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
  InterfereGraph interfere_graph_{initial, precolored};

  /// A mapping from a node to the list of moves it is associated with.
  TempNodeMap<MoveInstrSet> move_list{};
  /// When a move (u, v) is coalesced and v is put into coalesced_nodes, add
  /// alias[v] = u.
  TempNodeMap<TempNode> alias{};
  /// The color assigned to a node. For precolored nodes this is initialized at
  /// the very beginning.
  ColorMap color{};

  // Final result.
  std::unique_ptr<ra::Result> result_{};

  /**
   * @brief Basic workflow.
   *
   */

  void livenessAnalysis();
  void build_interfere_graph();
  void make_worklist();
  void simplify();
  void coalesce();
  void freeze();
  void select_spill();
  void assign_colors();
  void rewrite_program();
  void make_result();

  /**
   * @brief Helper functions.
   *
   */

  /// Get all valid moves related to n.
  MoveInstrSet node_moves(TempNode n) const;
  bool move_related(TempNode n) const;
  /// Get all actual adjacent node for n.
  TempNodeSet adjacent(TempNode n) const;
  void decrement_degree(TempNode m);
  void enable_moves(const TempNodeSet &nodes);
  TempNode get_alias(TempNode n) const;
  void add_worklist(TempNode u);
  bool OK(TempNode t, TempNode r) const;
  bool Briggs(TempNode u, TempNode v) const;
  bool Conservative(const TempNodeSet &nodes) const;
  void combine(TempNode u, TempNode v);
  void freeze_moves(TempNode u);

  /**
   * @brief Other functions.
   *
   */

  /**
   * @brief Extract temps from the instruction.
   *
   * @return std::vector<std::tuple<TempNode, bool>> A list of temporaries. The
   * second value indicates whether the temporary register is precolored.
   */
  [[deprecated]] static std::vector<std::tuple<TempNode, bool>>
  extract_temps(assem::Instr *);

  /**
   * Translates a MoveInstr into a pair of TempNodes.
   *
   * @param moveInstr The MoveInstr to be translated.
   * @return A pair of TempNodes representing the translated MoveInstr.
   */
  static std::pair<TempNode, TempNode> translate_move_instr(assem::MoveInstr *);

  /**
   * Selects a spill candidate using a heuristic algorithm.
   *
   * @return The TempNode representing the selected spill candidate.
   */
  TempNode heuristic_select_spill() const;

  static TempNodeSet retrieve_general_registers();

  static frame::Immediate drag_offset(frame::Access *access);
};

} // namespace ra

#endif