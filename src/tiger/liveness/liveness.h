#ifndef TIGER_LIVENESS_LIVENESS_H_
#define TIGER_LIVENESS_LIVENESS_H_

#include "tiger/codegen/assem.h"
#include "tiger/frame/temp.h"
#include "tiger/frame/x64frame.h"
#include "tiger/liveness/flowgraph.h"
#include "tiger/util/graph.h"

namespace live {

using INode = graph::Node<temp::Temp>;
using INodePtr = graph::Node<temp::Temp> *;
using INodeList = graph::NodeList<temp::Temp>;
using INodeListPtr = graph::NodeList<temp::Temp> *;
using IGraph = graph::Graph<temp::Temp>;
using IGraphPtr = graph::Graph<temp::Temp> *;

struct LivenessResult {
public:
  LivenessResult() = default;
  LivenessResult(
      std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> in,
      std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> out)
      : in_(std::move(in)), out_(std::move(out)) {}

  LivenessResult(const LivenessResult &result) = delete;
  LivenessResult(LivenessResult &&result) = delete;

  temp::TempList *out(graph::Node<assem::Instr> *node) const noexcept {
    auto res = this->out_->Look(node);
    assert(res);
    return res;
  }
  temp::TempList *in(graph::Node<assem::Instr> *node) const noexcept {
    auto res = this->in_->Look(node);
    assert(res);
    return res;
  }

private:
  std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> in_;
  std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> out_;
};

class LiveGraphFactory {
public:
  /**
   * @brief Take a flow graph and calculate liveness later.
   *
   * @param flowgraph
   */
  explicit LiveGraphFactory(fg::FGraphPtr flowgraph)
      : flowgraph_(flowgraph),
        in_(std::make_unique<graph::Table<assem::Instr, temp::TempList>>()),
        out_(std::make_unique<graph::Table<assem::Instr, temp::TempList>>()) {}
  /**
   * @brief Trigger liveness analysis.
   *
   */
  void Liveness();

  LivenessResult *TransferResult() {
    return new LivenessResult{std::move(in_), std::move(out_)};
  }

private:
  // Input: the flow graph
  fg::FGraphPtr flowgraph_;

  std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> in_;
  std::unique_ptr<graph::Table<assem::Instr, temp::TempList>> out_;

  void LiveMap();

  temp::TempList *get_out_(graph::Node<assem::Instr> *node) const noexcept;
  temp::TempList *get_in_(graph::Node<assem::Instr> *node) const noexcept;
};

} // namespace live

#endif