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
  assert(std::is_sorted(begin(lhs), end(lhs), Cmp{}));
  assert(std::is_sorted(begin(rhs), end(rhs), Cmp{}));
  std::set<T, Cmp> res{};
  std::set_union(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                 std::inserter(res, res.begin()), Cmp{});
  return res;
}

template <typename T, typename Cmp>
std::set<T, Cmp> set_diff(const std::set<T, Cmp> &lhs,
                          const std::set<T, Cmp> &rhs) {
  assert(std::is_sorted(begin(lhs), end(lhs), Cmp{}));
  assert(std::is_sorted(begin(rhs), end(rhs), Cmp{}));
  std::set<T, Cmp> res{};
  std::set_difference(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                      std::inserter(res, res.begin()), Cmp{});
  return res;
}

template <typename T, typename Cmp>
std::set<T, Cmp> set_intersect(const std::set<T, Cmp> &lhs,
                               const std::set<T, Cmp> &rhs) {
  assert(std::is_sorted(begin(lhs), end(lhs), Cmp{}));
  assert(std::is_sorted(begin(rhs), end(rhs), Cmp{}));
  std::set<T, Cmp> res{};
  std::set_intersection(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(),
                        std::inserter(res, res.begin()), Cmp{});
  return res;
}

class RegAllocator {

public:
  static constexpr int KColors = static_cast<int>(frame::Register::COUNT) - 2;

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
  /// Registers that have been coalesced.
  /// When u <- v is coalesced, v is put into it and u is put back into some
  /// worklist.
  TempNodeSet coalesced_nodes{};
  /// Nodes successfully colored.
  TempNodeSet colored_nodes{};
  struct {
    const TempNodeSet &get_set() const & { return set_; }
    void push(TempNode n) {
      assert(set_.count(n) == 0);
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

  using NodePair = std::pair<TempNode, TempNode>;
  struct NodePairHash {
    std::size_t operator()(const NodePair &pair) const {
      return pair.first->Int() * 100 + pair.second->Int();
    }
  };
  /**
   * @brief The set of interference edges (u, v) in the graph.
   * Tell if u and v are adjacent quickly.
   *
   * @note DO NOT access the data structure directly. @sa #is_interfere
   *
   * @note (u, v) in adjSet <=> (v, u) in adjSet.
   */
  std::unordered_set<NodePair, NodePairHash> adj_set_{};
  /**
   * @brief Adjacency list representation of the graph.
   * Get all the nodes adjacent to node X.
   *
   * @note DO NOT access the data structure directly. @sa #get_adj_of
   *
   */
  TempNodeMap<TempNodeSet> adj_list_{};
  /**
   * @brief Current degree of each node.
   *
   * @note degree[n] == adjList[n].size isn't satisfied all the time.
   * @note DO NOT access the data structure directly. @sa #get_degree
   * #set_degree
   *
   */
  TempNodeMap<std::size_t> degree_{};

  /**
   * @brief Get the degree of any node. Gracefully handles precolored nodes.
   *
   * @param n
   * @return the degree of n, or infinite if n is precolored.
   */
  std::size_t get_degree(TempNode n) const;

  /**
   * @brief Sets the degree of a given TempNode.
   *
   * @param n The TempNode whose degree is to be set. Should not be precolored.
   * @param d The degree value to be set.
   */
  void set_degree(TempNode n, std::size_t d);

  const TempNodeSet &get_adj_of(TempNode n) const;

  /**
   * @brief Efficiently checks if two TempNodes interfere with each other.
   *
   * This method uses #adj_set_ to query the interference edge.
   *
   * @param u The first TempNode.
   * @param v The second TempNode.
   * @note The order of (u, v) doesn't matter.
   * @return True if u and v interfere with each other, false otherwise.
   */
  bool is_interfere(TempNode u, TempNode v) const;

  /// A mapping from a node to the list of moves it is associated with.
  TempNodeMap<MoveInstrSet> move_list{};
  /// When a move (u, v) is coalesced and v is put into coalesced_nodes, add
  /// alias[v] = u.
  TempNodeMap<TempNode> alias{};
  /// The color assigned to a node. For precolored nodes this is initialized at
  /// the very beginning.
  ColorMap color{};
  ColorMap initial_color{};

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
  void clear();
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
  void remove_redundant_moves();

  void add_edge(TempNode u, TempNode v);

  /**
   * @brief Other functions.
   *
   */

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

  /// Whether the instruction is a move instruction.
  static bool is_move_instr(assem::Instr *);

  /// Whether the register is a machine register ().
  bool is_precolored(TempNode n) const;
};

} // namespace ra

#endif