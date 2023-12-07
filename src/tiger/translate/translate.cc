#include "tiger/translate/translate.h"

#include <queue>
#include <tiger/absyn/absyn.h>

#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/frame/x64frame.h"

/**
 * All the remembered fragments go into a private fragment list within
 * Translate.
 */
extern frame::Frags *frags;
extern frame::RegManager *reg_manager;

/**
 * @brief Imported from semant.cc.
 *
 * The namespace defines some utilities used during type-checking semant.
 *
 */
namespace type_check {

template <typename T> struct is_d_type {
  bool operator()(type::Ty *ty) const { return typeid(T) == typeid(*ty); }
};

template <typename T> struct is_d_entry {
  bool operator()(env::EnvEntry *ent) const {
    return typeid(T) == typeid(*ent);
  }
};

constexpr auto is_void_type = is_d_type<type::VoidTy>{};
constexpr auto is_int_type = is_d_type<type::IntTy>{};
constexpr auto is_name_type = is_d_type<type::NameTy>{};
constexpr auto is_array_type = is_d_type<type::ArrayTy>{};
constexpr auto is_record_type = is_d_type<type::RecordTy>{};
constexpr auto is_str_type = is_d_type<type::StringTy>{};
constexpr auto is_nil_type = is_d_type<type::NilTy>{};
constexpr auto is_fun_entry = is_d_entry<env::FunEntry>{};
constexpr auto is_var_entry = is_d_entry<env::VarEntry>{};
/**
 * @brief IS_SAME_DYNAMICAL_TYPE check if the two types are actually identical.
 *
 * It's just an alias of the static method type::Ty#IsSameType.
 *
 */
constexpr bool (*is_same_d_type)(type::Ty *,
                                 type::Ty *) = &type::Ty::IsSameType;
class DirectGraph {
public:
  explicit DirectGraph(std::size_t n) : adj_list_(n) {}
  ~DirectGraph() = default;
  void add_node(std::size_t from, std::size_t to) {
    adj_list_.at(from).push_front(to);
  }

  bool has_circuit() const {
    const std::size_t N = adj_list_.size();
    /**
     * We can always find the topological sorting in a DAG.
     */
    std::vector<std::size_t> topo_sorted{};
    topo_sorted.reserve(N);

    std::vector<int> in_degree(N);
    // Calculate the in-degree of each node
    for (const auto &adj : adj_list_) {
      for (auto end_n : adj) {
        in_degree[end_n]++;
      }
    }

    std::queue<std::size_t> zero_in_degree_q{};
    for (std::size_t i = 0; i < N; ++i) {
      if (in_degree[i] == 0) {
        zero_in_degree_q.push(i);
      }
    }
    // Kahn
    while (!zero_in_degree_q.empty()) {
      auto cur = zero_in_degree_q.front();
      topo_sorted.push_back(cur);
      zero_in_degree_q.pop();

      for (auto end_n : adj_list_.at(cur)) {
        if (--in_degree[end_n] == 0) {
          zero_in_degree_q.push(end_n);
        }
      }
    }
    // The algo shuts down before all nodes are pushed, which implies a circuit.
    return topo_sorted.size() != N;
  }

private:
  /**
   * @brief The directed graph of the type/function declaration sequence.
   *
   * If the specific definition of a type/function refers to another
   * type/function in the sequence of mutually recursive types/functions, then a
   * directed edge is added to the graph.
   */
  std::vector<std::list<std::size_t>> adj_list_;
};
} // namespace type_check

namespace frame {

tree::Exp *externalCall(std::string_view callee_label, tree::ExpList *args) {
  return new tree::CallExp(
      new tree::NameExp(temp::Label::UniqueSymbol(callee_label)), args);
}
} // namespace frame

namespace tr {

// [[deprecated("not used")]]
static constexpr type::Ty *dummyType = nullptr;

struct Cx {
  PatchList trues_;
  PatchList falses_;
  tree::Stm *stm_;

  Cx(PatchList trues, PatchList falses, tree::Stm *stm)
      : trues_(trues), falses_(falses), stm_(stm) {}
};

/**
 * @brief Interface for tr::ExExp, tr::NxExp and tr::CxExp.
 *
 * The virtual class defines three converting functions, which are useful to
 * "unwrap" the data structures defined in `translate' module and result in
 * the converting results.
 * From the textbook (P156), the three conversion functions behave like simply
 * "stripping off the corresponding constructor". However, they should work no
 * matter what constructor has been used!
 *
 * @note It's an abstract class. Its derived classes (ExExp, NxExp, CxExp) are
 * visible only in the translate module, therefore declared in translate.cc.
 *
 * @example (P156)
 * Suppose we have a tr::CxExp e which represents `a>b|c<d`:
 *    e = new tr::CxExp(trues, falses, stm);
 * and we have an assignment statement to translate:
 *    flag := (a>b|c<d);
 * Then we can implement the assignment as:
 *    MOVE(TEMP_{flag}, e->UnEx());
 * Even though CxExp is there instead.
 *
 */
class Exp {
public:
  [[nodiscard]] virtual tree::Exp *UnEx() = 0;
  [[nodiscard]] virtual tree::Stm *UnNx() = 0;
  [[nodiscard]] virtual Cx UnCx(err::ErrorMsg *errormsg) = 0;

  virtual ~Exp() = default;

  /**
   * @brief Return a no_op expression as a placeholder for further usage.
   *
   * Use case example: returned in FunctionDec#Translate.
   *
   * @return Exp* A no_op expression.
   */
  static Exp *no_op();
};

/**
 * @brief A struct consists of IR expression of a Tiger expression and type
 * information.
 *
 * While type-checking, we only have to return `type::Ty*` after semantic
 * analysis. But now, `Translate` must return a new data structure `ExpAndTy`,
 * which includes the intermediate-representation translation of each Tiger
 * expression.
 *
 * @note The type is used as the return type of Translate interface in Lab5.
 * And since we have done type checking in the Traverse interface (Lab4),
 * factually we won't use the filed @c ty_ at all.
 * Therefore, returning such data structure is redundant.
 */
struct ExpAndTy {
  tr::Exp *exp_;
  type::Ty *ty_;

  ExpAndTy(tr::Exp *exp) : exp_(exp), ty_(dummyType) {}
  ExpAndTy(tr::Exp *exp, type::Ty *ty) : exp_(exp), ty_(ty) {}

  [[nodiscard]] static ExpAndTy *dummy() {
    return new ExpAndTy(tr::Exp::no_op(), type::IntTy::Instance());
  }
};

struct ExExp : public Exp {
  tree::Exp *exp_;

  /**
   * @brief An expression.
   *
   * @param exp
   */
  explicit ExExp(tree::Exp *exp) : exp_(exp) {}

  [[nodiscard]] tree::Exp *UnEx() override { return this->exp_; }
  [[nodiscard]] tree::Stm *UnNx() override {
    // ExpStm
    return new tree::ExpStm(this->exp_);
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    auto t = temp::LabelFactory::NewLabel();
    auto f = temp::LabelFactory::NewLabel();
    auto trues = PatchList({&t});
    auto falses = PatchList({&f});
    tree::Stm *cjump_stm = new tree::CjumpStm(tree::RelOp::NE_OP, this->exp_,
                                              new tree::ConstExp(0), t, f);
    return Cx(trues, falses, cjump_stm);
  }
};

struct NxExp : public Exp {
  tree::Stm *stm_;

  /**
   * @brief "no result", which corresponds a common statement of tree language.
   *
   * @param stm
   */
  explicit NxExp(tree::Stm *stm) : stm_(stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    return new tree::EseqExp(this->stm_, new tree::ConstExp(0));
  }
  [[nodiscard]] tree::Stm *UnNx() override { return this->stm_; }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override {
    // Never occur
    assert(0);
  }
};

struct CxExp : public Exp {
  Cx cx_;

  /**
   * @brief Conditional. It can be regarded as a wrapper of tr::Cx.
   *
   * In essence, create a tr::Cx with the arguments.
   *
   * @param trues the patch list
   * @param falses
   * @param stm the statement may jump to the true-label or the false-label, but
   * these labels have yet to be filled in.
   */
  CxExp(PatchList trues, PatchList falses, tree::Stm *stm)
      : cx_(trues, falses, stm) {}

  [[nodiscard]] tree::Exp *UnEx() override {
    // The finial result.
    auto r = temp::TempFactory::NewTemp();
    auto t = temp::LabelFactory::NewLabel();
    auto f = temp::LabelFactory::NewLabel();
    this->cx_.trues_.DoPatch(t);
    this->cx_.falses_.DoPatch(f);
    return new tree::EseqExp(
        new tree::MoveStm(new tree::TempExp(r), new tree::ConstExp(1)),
        new tree::EseqExp(
            this->cx_.stm_,
            new tree::EseqExp(
                new tree::LabelStm(f),
                new tree::EseqExp(new tree::MoveStm(new tree::TempExp(r),
                                                    new tree::ConstExp(0)),
                                  new tree::EseqExp(new tree::LabelStm(t),
                                                    new tree::TempExp(r))))));
  }
  [[nodiscard]] tree::Stm *UnNx() override {
    // Unknown
    assert(0);
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) override { return this->cx_; }
};

Exp *Exp::no_op() { return new tr::ExExp(new tree::ConstExp(0)); }

void ProgTr::Translate() {
  FillBaseTEnv();
  FillBaseVEnv();
  this->main_level_ = std::unique_ptr<tr::Level>{
      tr::Level::newBaseLevel(temp::Label::UniqueSymbol("tigermain"))};
  auto exp_and_ty = this->absyn_tree_->Translate(
      this->venv_.get(), this->tenv_.get(), this->main_level_.get(), nullptr,
      errormsg_.get());
  this->main_level_->procEntryExit(exp_and_ty->exp_, {});
}

tr::Level *tr::Level::newBaseLevel(temp::Label *name) {
  auto level = new Level();
  level->frame_ = frame::newFrame(name, {});
  level->parent_ = nullptr;
  return level;
}

tr::Access *tr::Level::allocLocal(bool escape) {
  auto raw_access = this->frame_->allocateLocal(escape);
  return new tr::Access(this, raw_access);
}

frame::Access *tr::Level::stackLink() const {
  return this->frame_->getFormals().front();
}

/**
 * @brief Utility function to follow static link.
 * More details: P160.
 *
 */
tr::Exp *makeSimpleVariable(tr::Access *access, tr::Level *level,
                            tree::Exp *framePointer) {
  if (access->level_ == level) {
    return new tr::ExExp(access->access_->toExp(framePointer));
  }
  // Follow static links, recursively.
  // If the compiler is well-defined, the access must not be escaped here.

  // Get the static link, which is the frame pointer of the next level.
  assert(level->parent_);
  auto fp_new =
      new tree::BinopExp(tree::BinOp::PLUS_OP, framePointer,
                         new tree::ConstExp(Level::KStaticLinkOffset));
  return makeSimpleVariable(access, level->parent_, fp_new);
}

tr::Exp *makeSimpleVariable(tr::Access *access, tr::Level *level) {
  // Initial frame pointer
  auto fp = reg_manager->FramePointer();
  return makeSimpleVariable(access, level, new tree::TempExp(fp));
}

tr::Exp *makeBinaryExp(absyn::Oper oper, tr::Exp *left, tr::Exp *right) {
  using absyn::Oper;
  static std::unordered_map<Oper, tree::BinOp> arithmetic_op_mapper = {
      // AND and OR need to be treated specially.
      {Oper::PLUS_OP, tree::BinOp::PLUS_OP},
      {Oper::MINUS_OP, tree::BinOp::MINUS_OP},
      {Oper::TIMES_OP, tree::BinOp::MUL_OP},
      {Oper::DIVIDE_OP, tree::BinOp::DIV_OP},
  };
  static std::unordered_map<Oper, tree::RelOp> relation_op_mapper = {
      {Oper::EQ_OP, tree::RelOp::EQ_OP}, {Oper::NEQ_OP, tree::RelOp::NE_OP},
      {Oper::LE_OP, tree::RelOp::LE_OP}, {Oper::LT_OP, tree::RelOp::LT_OP},
      {Oper::GE_OP, tree::RelOp::GE_OP}, {Oper::GT_OP, tree::RelOp::GT_OP},
  };

  if (auto arith_op_it = arithmetic_op_mapper.find(oper);
      arith_op_it != arithmetic_op_mapper.end()) {
    return new tr::ExExp(
        new tree::BinopExp(arith_op_it->second, left->UnEx(), right->UnEx()));
  } else if (auto rel_op_it = relation_op_mapper.find(oper);
             rel_op_it != relation_op_mapper.end()) {
    auto cjump_stm = new tree::CjumpStm(rel_op_it->second, left->UnEx(),
                                        right->UnEx(), nullptr, nullptr);
    auto trues = tr::PatchList({&cjump_stm->true_label_});
    auto falses = tr::PatchList({&cjump_stm->false_label_});
    return new tr::CxExp(trues, falses, cjump_stm);
  }

  // Handle short circuit
  if (oper == Oper::AND_OP) {
    // left & right => if left then right else 0
    return tr::makeIfThenElse(left, right, new tr::ExExp(new tree::ConstExp(0)),
                              nullptr); // FIXME
  } else if (oper == Oper::OR_OP) {
    // left | right => if left then 1 else right
    return tr::makeIfThenElse(left, new tr::ExExp(new tree::ConstExp(1)), right,
                              nullptr); // FIXME
  }

  assert(0);
}

tr::Exp *makeSequentialExp(std::list<tr::Exp *> expList) {
  if (expList.empty()) {
    assert(0);
    return tr::Exp::no_op();
  }

  auto first = expList.front();
  auto first_exp = first->UnNx();
  expList.pop_front();
  if (expList.empty()) {
    return first;
  }
  auto other_res = makeSequentialExp(std::move(expList));
  auto other_exp = other_res->UnEx();

  return new tr::ExExp(new tree::EseqExp(first_exp, other_exp));
}

[[nodiscard]] tr::Exp *makeIfThenElse(tr::Exp *test_e, tr::Exp *then_e,
                                      tr::Exp *else_e, err::ErrorMsg *err_msg) {
  // FIXME not efficient? (P165)
  // A new register to store the result.
  auto result_temp = temp::TempFactory::NewTemp();
  auto cjump_stm = test_e->UnCx(err_msg);
  tree::Stm *true_action = nullptr;
  tree::Stm *false_action = nullptr;
  if (else_e == nullptr) {
    true_action = then_e->UnNx();
    false_action = tr::Exp::no_op()->UnNx();
  } else {
    true_action =
        new tree::MoveStm(new tree::TempExp(result_temp), then_e->UnEx());
    false_action =
        new tree::MoveStm(new tree::TempExp(result_temp), else_e->UnEx());
  }
  assert(true_action && false_action);

  auto t = temp::LabelFactory::NewLabel();
  auto f = temp::LabelFactory::NewLabel();
  cjump_stm.trues_.DoPatch(t);
  cjump_stm.falses_.DoPatch(f);

  if (else_e == nullptr) {
    return new tr::NxExp(new tree::SeqStm(
        cjump_stm.stm_,
        new tree::SeqStm(
            new tree::LabelStm(f),
            new tree::SeqStm(
                false_action,
                new tree::SeqStm(new tree::LabelStm(t), true_action)))));
  } else {
    return new tr::ExExp(new tree::EseqExp(
        cjump_stm.stm_,
        new tree::EseqExp(
            new tree::LabelStm(f),
            new tree::EseqExp(
                false_action,
                new tree::EseqExp(
                    new tree::LabelStm(t),
                    new tree::EseqExp(true_action,
                                      new tree::TempExp(result_temp)))))));
  }
}

tr::Exp *makeField(tr::Exp *record, int index) {
  return new tr::ExExp(new tree::MemExp(
      new tree::BinopExp(tree::BinOp::PLUS_OP, record->UnEx(),
                         new tree::ConstExp(index * frame::KX64WordSize))));
}

tr::Exp *makeSubscript(tr::Exp *arr, tr::Exp *index) {
  return new tr::ExExp(new tree::MemExp(new tree::BinopExp(
      tree::BinOp::PLUS_OP, arr->UnEx(),
      new tree::BinopExp(tree::BinOp::MUL_OP, index->UnEx(),
                         new tree::ConstExp(frame::KX64WordSize)))));
}

tr::Exp *makeAssignment(tr::Exp *dst, tr::Exp *src) {
  return new tr::NxExp(new tree::MoveStm(dst->UnEx(), src->UnEx()));
}

tr::Exp *makeWhile(tr::Exp *test, tr::Exp *body, temp::Label *done,
                   err::ErrorMsg *errormsg) {
  auto test_l = temp::LabelFactory::NewLabel();
  auto body_l = temp::LabelFactory::NewLabel();
  // Store the condition.
  auto cjump = test->UnCx(errormsg);
  cjump.trues_.DoPatch(body_l);
  cjump.falses_.DoPatch(done);

  return new tr::NxExp(new tree::SeqStm(
      new tree::LabelStm(test_l),
      new tree::SeqStm(
          cjump.stm_,
          new tree::SeqStm(
              body->UnNx(),
              new tree::SeqStm(
                  new tree::JumpStm(new tree::NameExp(test_l),
                                    new std::vector<temp::Label *>{test_l}),
                  new tree::LabelStm(done))))));
}

tr::Exp *makeCall(temp::Label *fun_label, const std::list<tr::Exp *> &args) {
  auto arg_list = new tree::ExpList();
  for (auto arg : args) {
    arg_list->Append(arg->UnEx());
  }
  return new tr::ExExp(
      new tree::CallExp(new tree::NameExp(fun_label), arg_list));
}

tr::Exp *makeString(temp::Label *str_label) {
  return new tr::ExExp(new tree::NameExp(str_label));
}

tr::Exp *makeRecord(const std::vector<tr::Exp *> &field_exps) {
  std::list<tr::Exp *> init_record_stm{};
  auto malloc_call = frame::externalCall(
      "alloc_record",
      new tree::ExpList({new tree::ConstExp(init_record_stm.size())}));
  auto r = temp::TempFactory::NewTemp();
  auto create_record_stm = new tree::MoveStm(new tree::TempExp(r), malloc_call);
  init_record_stm.push_back(new tr::NxExp(create_record_stm));

  for (int i = 0; i < field_exps.size(); ++i) {
    auto init_field_stm =
        new tree::MoveStm(new tree::MemExp(new tree::BinopExp(
                              tree::BinOp::PLUS_OP, new tree::TempExp(r),
                              new tree::ConstExp(i * frame::KX64WordSize))),
                          field_exps.at(i)->UnEx());
    init_record_stm.push_back(new tr::NxExp(init_field_stm));
  }
  auto flatten_seq_stm = tr::makeSequentialExp(std::move(init_record_stm));
  return new tr::ExExp(
      new tree::EseqExp(flatten_seq_stm->UnNx(), new tree::TempExp(r)));
}

tr::Exp *makeArray(tr::Exp *size, tr::Exp *init) {
  auto init_call = frame::externalCall(
      "init_array", new tree::ExpList({size->UnEx(), init->UnEx()}));
  return new tr::ExExp(init_call);
}

tr::Exp *makeFunReturn(tr::Exp *body, bool is_procedure) {
  if (is_procedure) {
    return body;
  }
  return new tr::NxExp(new tree::MoveStm(
      new tree::TempExp(reg_manager->ReturnValue()), body->UnEx()));
}

tr::Exp *makeDirectJump(temp::Label *target) {
  return new tr::NxExp(new tree::JumpStm(
      new tree::NameExp(target), new std::vector<temp::Label *>{target}));
}

tr::Exp *makeNil() { return tr::makeConstant(0); }

tr::Exp *makeConstant(int i) { return new tr::ExExp(new tree::ConstExp(i)); }

tr::Exp *makeStringEqual(tr::Exp *str1, tr::Exp *str2) {
  auto args = new tree::ExpList{str1->UnEx(), str2->UnEx()};
  auto exp = frame::externalCall("string_equal", args);
  return new tr::ExExp(exp);
}

tr::Level *Level::newLevel(temp::Label *name, const std::list<bool> &formals) {
  auto new_Level = new tr::Level();
  std::list<bool> actual_formals(formals);
  // The static link must be escaped.
  actual_formals.push_front(true);
  new_Level->frame_ = frame::newFrame(name, actual_formals);
  new_Level->parent_ = this;
  return new_Level;
}

std::list<tr::Access *> Level::formals() {
  std::list<tr::Access *> visible_formals{};
  auto actual_formals = this->frame_->getFormals();
  // At least we have static link.
  assert(actual_formals.size() >= 1);

  transform(next(begin(actual_formals)), end(actual_formals),
            begin(visible_formals),
            [this](frame::Access *f_access) -> tr::Access * {
              return new tr::Access(this, f_access);
            });
  return visible_formals;
}

tr::Exp *Level::prepareStaticLink(tr::Level *level) const {
  if (level->parent_ == this) {
    // The function is defined in a nested level, which must satisfies
    // Lx = Lp + 1 (Lx: callee, Lp: caller). Otherwise, the function is not
    // visible for the current level.
    return new tr::ExExp(new tree::TempExp(reg_manager->FramePointer()));
  }
  // Lx <= Lp.
  // Though we can simply pass the frame pointer as the static link,
  // we may build the static link "statically" at compiler time.
  // The frame of which we need to get the frame pointer is the lowest common
  // ancestor of the caller the callee.
  std::list<tr::Exp *> move_stm_list{};
  auto r = temp::TempFactory::NewTemp();
  move_stm_list.push_back(new tr::NxExp(new tree::MoveStm(
      new tree::TempExp(r),
      new tree::BinopExp(tree::BinOp::PLUS_OP,
                         new tree::TempExp(reg_manager->FramePointer()),
                         new tree::ConstExp(KStaticLinkOffset)))));

  auto cur_level = this;
  while (cur_level != level->parent_) {
    assert(cur_level);
    move_stm_list.push_back(new tr::NxExp(new tree::MoveStm(
        new tree::TempExp(r),
        new tree::BinopExp(tree::BinOp::PLUS_OP, new tree::TempExp(r),
                           new tree::ConstExp(KStaticLinkOffset)))));
    cur_level = cur_level->parent_;
  }
  auto flatten_exp = tr::makeSequentialExp(std::move(move_stm_list));
  return new tr::ExExp(
      new tree::EseqExp(flatten_exp->UnNx(), new tree::TempExp(r)));
}

void tr::Level::procEntryExit(tr::Exp *body,
                              const std::list<tr::Access *> &formals) {
  // Push proc into global list.
  auto stm1 = this->frame_->procEntryExit1(body->UnNx());
  frags->PushBack(new frame::ProcFrag(stm1, this->frame_));
}

int tr::Level::KStaticLinkOffset = 0 * frame::KX64WordSize;

} // namespace tr

// Translation is done in the semantic analysis phase of Tiger compiler.
namespace absyn {

tr::ExpAndTy *AbsynTree::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();
  auto res = this->root_->Translate(venv, tenv, level, done, errormsg);
  tenv->EndScope();
  venv->EndScope();
  return res;
}

tr::ExpAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  auto entry = venv->Look(this->sym_);

  if (!(entry && type_check::is_var_entry(entry))) {
    errormsg->Error(this->pos_, "undefined variable %s",
                    this->sym_->Name().data());
    return tr::ExpAndTy::dummy();
  }

  auto var_entry = static_cast<env::VarEntry *>(entry);
  auto exp = tr::makeSimpleVariable(var_entry->access_, level);
  return new tr::ExpAndTy(exp, var_entry->ty_);
}

tr::ExpAndTy *FieldVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *done,
                                  err::ErrorMsg *errormsg) const {
  auto var_exp_and_ty =
      this->var_->Translate(venv, tenv, level, done, errormsg);

  auto var_ty = var_exp_and_ty->ty_;
  assert(var_ty);

  if (!type_check::is_record_type(var_ty)) {
    errormsg->Error(this->pos_, "not a record type");
    return tr::ExpAndTy::dummy();
  }
  auto field_list = static_cast<type::RecordTy *>(var_ty)->fields_->GetList();
  auto found_field_it = std::find_if(
      field_list.begin(), field_list.end(), [this](type::Field *field) -> bool {
        return field->name_->Name() == this->sym_->Name();
      });
  if (found_field_it == field_list.end()) {
    errormsg->Error(this->pos_, "field %s doesn't exist",
                    this->sym_->Name().c_str());
    return tr::ExpAndTy::dummy();
  }
  int field_index = std::distance(field_list.begin(), found_field_it);

  auto ret_ty = (*found_field_it)->ty_->ActualTy();
  auto ret_exp = tr::makeField(var_exp_and_ty->exp_, field_index);

  return new tr::ExpAndTy(ret_exp, ret_ty);
}

tr::ExpAndTy *SubscriptVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                      tr::Level *level, temp::Label *done,
                                      err::ErrorMsg *errormsg) const {
  auto array_exp_and_ty =
      this->var_->Translate(venv, tenv, level, done, errormsg);
  auto array_ty = array_exp_and_ty->ty_;

  if (!type_check::is_array_type(array_ty)) {
    errormsg->Error(this->pos_, "array type required");
    return tr::ExpAndTy::dummy();
  }
  auto index_exp_and_ty =
      this->subscript_->Translate(venv, tenv, level, done, errormsg);
  auto index_ty = index_exp_and_ty->ty_;

  if (!type_check::is_int_type(index_ty)) {
    errormsg->Error(this->pos_, "integer required");
  }
  auto ret_ty = static_cast<type::ArrayTy *>(array_ty)->ty_->ActualTy();
  auto ret_exp =
      tr::makeSubscript(array_exp_and_ty->exp_, index_exp_and_ty->exp_);

  return new tr::ExpAndTy(ret_exp, ret_ty);
}

tr::ExpAndTy *VarExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  return this->var_->Translate(venv, tenv, level, done, errormsg);
}

tr::ExpAndTy *NilExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(tr::makeNil(), type::NilTy::Instance());
}

tr::ExpAndTy *IntExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(tr::makeConstant(this->val_),
                          type::IntTy::Instance());
}

tr::ExpAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  auto str_l = temp::LabelFactory::NewLabel();
  frags->PushBack(new frame::StringFrag(str_l, this->str_));
  return new tr::ExpAndTy(tr::makeString(str_l), type::StringTy::Instance());
}

tr::ExpAndTy *CallExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *done,
                                 err::ErrorMsg *errormsg) const {
  // TODO type checking
  auto entry = venv->Look(this->func_);
  assert(entry && type_check::is_fun_entry(entry));

  auto fun_entry = static_cast<env::FunEntry *>(entry);
  std::list<tr::Exp *> call_args_exp{};
  // Translate knows that each frame contains a static link. (P144)
  call_args_exp.push_back(level->prepareStaticLink(fun_entry->level_));
  for (auto arg : this->args_->GetList()) {
    auto arg_exp_and_ty = arg->Translate(venv, tenv, level, done, errormsg);
    call_args_exp.push_back(arg_exp_and_ty->exp_);
  }
  auto ret_exp = tr::makeCall(fun_entry->label_, call_args_exp);
  auto ret_ty = fun_entry->result_;
  return new tr::ExpAndTy(ret_exp, ret_ty);
}

tr::ExpAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *done,
                               err::ErrorMsg *errormsg) const {
  auto left_exp_and_ty =
      this->left_->Translate(venv, tenv, level, done, errormsg);
  auto right_exp_and_ty =
      this->right_->Translate(venv, tenv, level, done, errormsg);

  auto lhs_ty = left_exp_and_ty->ty_;
  auto rhs_ty = right_exp_and_ty->ty_;

  switch (this->oper_) {
  // Comparison
  // Equal and no-equal: also on the record/arr type, compare by "reference"
  case Oper::EQ_OP:
  case Oper::NEQ_OP:
    if (!type_check::is_same_d_type(lhs_ty, rhs_ty)) {
      errormsg->Error(this->pos_, "same type required");
      return tr::ExpAndTy::dummy();
    }
    if (type_check::is_str_type(lhs_ty->ActualTy())) {
      if (this->oper_ == Oper::EQ_OP) {
        return new tr::ExpAndTy(
            tr::makeStringEqual(left_exp_and_ty->exp_, right_exp_and_ty->exp_),
            type::StringTy::Instance());
      }
      auto newAbsynExp = new absyn::OpExp(
          0, Oper::MINUS_OP, new absyn::IntExp(0, 1),
          new absyn::OpExp(0, Oper::EQ_OP, this->left_, this->right_));
      return newAbsynExp->Translate(venv, tenv, level, done, errormsg);
    }
    // str1 != str2   =>  1 - (str1 == str2)
    break;
  case Oper::PLUS_OP:
  case Oper::MINUS_OP:
  case Oper::TIMES_OP:
  case Oper::DIVIDE_OP:
  case Oper::LE_OP:
  case Oper::GE_OP:
  case Oper::LT_OP:
  case Oper::GT_OP:
    if (!(type_check::is_int_type(lhs_ty) && type_check::is_int_type(rhs_ty))) {
      errormsg->Error(this->pos_, "integer required");
      return tr::ExpAndTy::dummy();
    }
  default:
    // TODO check more?
    break;
  }

  auto ret_exp = tr::makeBinaryExp(this->oper_, left_exp_and_ty->exp_,
                                   right_exp_and_ty->exp_);
  return new tr::ExpAndTy(ret_exp, type::IntTy::Instance());
}

tr::ExpAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  // TODO type checking
  auto entry = tenv->Look(this->typ_);
  assert(entry && type_check::is_record_type(entry->ActualTy()));

  auto record_ty = static_cast<type::RecordTy *>(entry);

  auto field_list = this->fields_->GetList();
  std::vector<tr::Exp *> field_exps{};

  // From Tiger manual: The field names and types of the record expression must
  // match those of the named type, in the order given. (P524)
  for (auto field : field_list) {
    auto field_exp_and_ty =
        field->exp_->Translate(venv, tenv, level, done, errormsg);
    field_exps.push_back(field_exp_and_ty->exp_);
  }
  auto ret_exp = tr::makeRecord(field_exps);
  return new tr::ExpAndTy(ret_exp, record_ty);
}

tr::ExpAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  std::list<tr::Exp *> expList{};

  assert(!this->seq_->GetList().empty());
  for (auto exp : this->seq_->GetList()) {
    auto ret = exp->Translate(venv, tenv, level, done, errormsg);
    expList.push_back(ret->exp_);
  }
  auto res_exp = tr::makeSequentialExp(std::move(expList));
  return new tr::ExpAndTy(res_exp);
}

tr::ExpAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *done,
                                   err::ErrorMsg *errormsg) const {
  /// Bypass that "ugly coupling". @sa absyn::AssignExp#SemAnalyze.
  // Something that check "loop variable can't be assigned".

  auto lvalue_exp_and_ty =
      this->var_->Translate(venv, tenv, level, done, errormsg);
  auto assign_to_exp_and_ty =
      this->exp_->Translate(venv, tenv, level, done, errormsg);

  if (!type_check::is_same_d_type(lvalue_exp_and_ty->ty_,
                                  assign_to_exp_and_ty->ty_)) {
    errormsg->Error(this->pos_, "unmatched assign exp");
  }
  return new tr::ExpAndTy(
      tr::makeAssignment(lvalue_exp_and_ty->exp_, assign_to_exp_and_ty->exp_),
      type::VoidTy::Instance());
}

tr::ExpAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *done,
                               err::ErrorMsg *errormsg) const {
  // TODO type-checking
  auto test_exp_and_ty =
      this->test_->Translate(venv, tenv, level, done, errormsg);
  auto then_exp_and_ty =
      this->then_->Translate(venv, tenv, level, done, errormsg);

  auto else_exp_and_ty =
      this->elsee_ == nullptr
          ? nullptr
          : this->elsee_->Translate(venv, tenv, level, done, errormsg);

  auto res_exp = tr::makeIfThenElse(
      test_exp_and_ty->exp_, then_exp_and_ty->exp_,
      (else_exp_and_ty == nullptr ? nullptr : else_exp_and_ty->exp_), errormsg);

  return new tr::ExpAndTy(res_exp);
}

tr::ExpAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *done,
                                  err::ErrorMsg *errormsg) const {
  auto test_exp_and_ty =
      this->test_->Translate(venv, tenv, level, done, errormsg);

  done = temp::LabelFactory::NewLabel();

  auto body_exp_any_ty =
      this->body_->Translate(venv, tenv, level, done, errormsg);

  return new tr::ExpAndTy(tr::makeWhile(test_exp_and_ty->exp_,
                                        body_exp_any_ty->exp_, done, errormsg),
                          type::VoidTy::Instance());
}

/**
 * Pseudocode of the for expression:
 *
 * ```
 * if i > limit goto done
 *    body
 * if i == limit goto done
 * Loop:
 *    i := i + 1
 *    body
 *    if i <= limit goto Loop
 * done:
 * ```
 *
 * ```
 * let var i := lo
 *     var limit := hi
 * in while i <= limit
 *     do (body; i := i + 1)
 * end
 * ```
 *
 */
tr::ExpAndTy *ForExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  // FIXME memory leak
  auto decList = new DecList();
  auto limit_symbol =
      sym::Symbol::UniqueSymbol(this->var_->Name() + "__limit__");
  auto i_dec = new VarDec(0, limit_symbol, nullptr, this->hi_);
  i_dec->escape_ = this->escape_;
  decList->Prepend(i_dec);
  auto limit_dec = new VarDec(0, this->var_, nullptr, this->lo_);
  limit_dec->escape_ = this->escape_;
  decList->Prepend(limit_dec);

  auto test_exp =
      new OpExp(0, Oper::LE_OP, new VarExp(0, new SimpleVar(0, this->var_)),
                new VarExp(0, new SimpleVar(0, limit_symbol)));
  auto increase_exp = new AssignExp(
      0, new SimpleVar(0, this->var_),
      new OpExp(0, Oper::PLUS_OP, new VarExp(0, new SimpleVar(0, this->var_)),
                new IntExp(0, 1)));
  auto body_exp_list = new ExpList();
  body_exp_list->Prepend(increase_exp);
  body_exp_list->Prepend(test_exp);
  auto while_body = new SeqExp(0, body_exp_list);

  auto let_exp = new LetExp(0, decList, new WhileExp(0, test_exp, while_body));

  return let_exp->Translate(venv, tenv, level, done, errormsg);
}

tr::ExpAndTy *BreakExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *done,
                                  err::ErrorMsg *errormsg) const {
  if (done == nullptr) {
    errormsg->Error(this->pos_, "break is not inside any loop");
    return tr::ExpAndTy::dummy();
  }
  return new tr::ExpAndTy(tr::makeDirectJump(done), type::VoidTy::Instance());
}

tr::ExpAndTy *LetExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();
  std::list<tr::Exp *> exps{};
  for (auto dec : this->decs_->GetList()) {
    auto dec_exp = dec->Translate(venv, tenv, level, done, errormsg);
    exps.push_back(dec_exp);
  }
  auto body_exp_and_ty =
      this->body_->Translate(venv, tenv, level, done, errormsg);
  venv->EndScope();
  tenv->EndScope();
  exps.push_back(body_exp_and_ty->exp_);
  return new tr::ExpAndTy(tr::makeSequentialExp(std::move(exps)),
                          body_exp_and_ty->ty_);
}

tr::ExpAndTy *ArrayExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *done,
                                  err::ErrorMsg *errormsg) const {
  // TODO type-checking
  auto entry = tenv->Look(this->typ_);
  assert(entry && type_check::is_array_type(entry->ActualTy()));
  auto array_ty = static_cast<type::ArrayTy *>(entry);

  auto init_exp_and_ty =
      this->init_->Translate(venv, tenv, level, done, errormsg);
  auto size_exp_and_ty =
      this->size_->Translate(venv, tenv, level, done, errormsg);
  auto ret_exp = tr::makeArray(size_exp_and_ty->exp_, init_exp_and_ty->exp_);

  return new tr::ExpAndTy(ret_exp, array_ty);
}

tr::ExpAndTy *VoidExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *done,
                                 err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(tr::Exp::no_op(), type::VoidTy::Instance());
}

/**
 * @brief Translate a Tiger function definition.
 *
 * (P173)
 * 1. Call upon tr::Level::newLevel in processing a function header.
 * 2. Call other interfaces in tr to translate the function body, which has
 *    the side effect of remembering fragments for any string literals.
 * 3. Call tr::procEntryExit, which has the side effect of remembering a
 *    ProgFrag.
 *
 * All the remembered fragments go into a private fragment list within
 * Translate.
 *
 * @param venv
 * @param tenv
 * @param level
 * @param label
 * @param errormsg
 * @return tr::Exp*
 */
tr::Exp *FunctionDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *done,
                                err::ErrorMsg *errormsg) const {
  // TODO type-checking
  auto fun_list = this->functions_->GetList();

  // Enter the headers.
  for (auto fun : fun_list) {
    // analyze the formals and result
    auto formals = fun->params_->MakeFormalTyList(tenv, errormsg);
    std::list<bool> formal_escapes{};
    for (auto param : fun->params_->GetList()) {
      formal_escapes.push_back(param->escape_);
    }
    auto fun_label = temp::LabelFactory::NamedLabel(fun->name_->Name());
    // If fun->result_ == nullptr, it declare a **procedure**.
    auto result_ty =
        fun->result_ ? tenv->Look(fun->result_) : type::VoidTy::Instance();
    auto fun_level = level->newLevel(fun_label, formal_escapes);
    auto fun_entry =
        new env::FunEntry(fun_level, fun_label, formals, result_ty);
    // Leave the function body untouched.
    venv->Enter(fun->name_, fun_entry);
  }

  for (auto fun : fun_list) {
    auto entry = venv->Look(fun->name_);
    assert(entry && type_check::is_fun_entry(entry));
    auto fun_entry = static_cast<env::FunEntry *>(entry);
    venv->BeginScope();
    // Enter the formals
    {
      auto param_list = fun->params_->GetList();
      auto formals = fun_entry->formals_->GetList();
      auto access_list = fun_entry->level_->formals();
      assert(param_list.size() == formals.size() &&
             param_list.size() == access_list.size());

      auto param_it = param_list.begin();
      auto formal_it = formals.begin();
      auto access_it = access_list.begin();

      while (param_it != param_list.end()) {
        venv->Enter((*param_it)->name_,
                    new env::VarEntry(*access_it, (*formal_it)->ActualTy()));
        param_it++;
        formal_it++;
        access_it++;
      }
      assert(formal_it == formals.end() && access_it == access_list.end());
    }

    auto body_exp_and_ty =
        fun->body_->Translate(venv, tenv, level, done, errormsg);
    // body_exps.push_back(body_exp_and_ty->exp_);
    // body_is_procedures.push_back(fun->result_ == nullptr);
    bool is_procedure = type_check::is_void_type(fun_entry->result_);
    auto body_exp = tr::makeFunReturn(body_exp_and_ty->exp_, is_procedure);

    fun_entry->level_->procEntryExit(body_exp, fun_entry->level_->formals());

    venv->EndScope();
  }

  return tr::Exp::no_op();
}

tr::Exp *VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                           tr::Level *level, temp::Label *done,
                           err::ErrorMsg *errormsg) const {
  // var decl in Tiger language must give an init.
  assert(this->init_);

  auto ty_entry = this->typ_ ? tenv->Look(this->typ_) : nullptr;
  if (this->typ_ && !ty_entry) {
    errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
  }

  auto init_exp_and_ty =
      this->init_->Translate(venv, tenv, level, done, errormsg);

  auto init_ty = init_exp_and_ty->ty_;
  if (this->typ_ && !type_check::is_same_d_type(ty_entry, init_ty)) {
    errormsg->Error(this->pos_, "type mismatch");
  }
  if (!this->typ_ && type_check::is_nil_type(init_ty)) {
    errormsg->Error(this->pos_,
                    "init should not be nil without type specified");
  }

  // Allocate a local variable in the frame (increase the frame size).
  auto access = level->allocLocal(this->escape_);
  auto dst_exp = tr::makeSimpleVariable(access, level);

  // Update var env
  venv->Enter(this->var_, new env::VarEntry(access, init_ty));

  // Initialization action.
  auto init_exp = tr::makeAssignment(dst_exp, init_exp_and_ty->exp_);
  return init_exp;
}

/**
 * type declaration: identical to lab4 type-checking.
 */
tr::Exp *TypeDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                            tr::Level *level, temp::Label *done,
                            err::ErrorMsg *errormsg) const {
  auto a_named_type_list = this->types_->GetList();
  std::vector<type::NameTy *> s_named_type_list{};
  // Should be a DAG
  type_check::DirectGraph dg{a_named_type_list.size()};
  s_named_type_list.reserve(a_named_type_list.size());

  // Enter the "headers"
  for (auto type : a_named_type_list) {
    // Check duplicate
    auto maybe_dup =
        std::find_if(s_named_type_list.begin(), s_named_type_list.end(),
                     [type](type::NameTy *ty) -> bool {
                       return ty->sym_->Name() == type->name_->Name();
                     });
    if (maybe_dup != s_named_type_list.end()) {
      errormsg->Error(this->pos_, "two types have the same name");
      return tr::Exp::no_op();
    }

    auto s_named_ty = new type::NameTy{type->name_, nullptr};
    // Travel the header list to find if there is the same header.
    tenv->Enter(type->name_, s_named_ty);
    s_named_type_list.push_back(s_named_ty);
  }
  {
    auto a_it = a_named_type_list.begin();
    auto s_it = s_named_type_list.begin();
    while (a_it != a_named_type_list.end()) {
      auto actual_ty = (*a_it)->ty_->SemAnalyze(tenv, errormsg);
      (*s_it)->ty_ = actual_ty;

      if (type_check::is_name_type(actual_ty)) {

        auto found_it = std::find_if(
            s_named_type_list.begin(), s_named_type_list.end(),
            [actual_ty](type::NameTy *ty) -> bool {
              return ty->sym_->Name() ==
                     static_cast<type::NameTy *>(actual_ty)->sym_->Name();
            });

        if (found_it != s_named_type_list.end()) {
          // Add the edge.
          auto begin_n = s_it - s_named_type_list.begin();
          auto end_n = found_it - s_named_type_list.begin();
          dg.add_node(begin_n, end_n);
        }
        /**
         * The name type isn't in the mutually recursive types,
         * so there is no need to add an edge.
         */
      }

      s_it++;
      a_it++;
    }
    assert(s_it == s_named_type_list.end());
  }
  if (dg.has_circuit()) {
    errormsg->Error(this->pos_, "illegal type cycle");
  }

  return tr::Exp::no_op();
}

type::Ty *NameTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  auto entry = tenv->Look(this->name_);
  if (!entry) {
    errormsg->Error(this->pos_, "undefined type %s",
                    this->name_->Name().data());
    return type::VoidTy::Instance();
  }
  // From the book P121:
  // It’s important that transTy stop as soon as it gets to any Ty_Name type.
  // That is, we should not get the actual type since it may be NULL.
  return entry;
}

type::Ty *RecordTy::Translate(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  auto field_list = this->record_->GetList();
  auto field_ty_list = new type::FieldList{};
  for (auto field : field_list) {
    auto entry = tenv->Look(field->typ_);
    if (!entry) {
      errormsg->Error(this->pos_, "undefined type %s",
                      field->typ_->Name().data());
    }
    field_ty_list->Append(new type::Field(field->name_, entry));
  }
  return new type::RecordTy(field_ty_list);
}

type::Ty *ArrayTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  auto entry = tenv->Look(this->array_);
  if (!entry) {
    errormsg->Error(this->pos_, "undefined type %s",
                    this->array_->Name().data());
    return new type::ArrayTy{type::IntTy::Instance()};
  }
  return new type::ArrayTy{entry->ActualTy()};
}

} // namespace absyn