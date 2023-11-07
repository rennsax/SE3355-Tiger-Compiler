#include "tiger/semant/semant.h"
#include "tiger/absyn/absyn.h"
#include <queue>

namespace absyn {

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
constexpr auto is_nil_type = is_d_type<type::NilTy>{};
constexpr auto is_fun_entry = is_d_entry<env::FunEntry>{};
constexpr auto is_var_entry = is_d_entry<env::VarEntry>{};
constexpr bool (*is_same_d_type)(type::Ty *,
                                 type::Ty *) = &type::Ty::IsSameType;
// constexpr auto is_same_d_type = [](type::Ty* t1, type::Ty* t2) -> bool {
//   return typeid(*t1) == typeid(*t2);
// };

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

void AbsynTree::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                           err::ErrorMsg *errormsg) const {
  // venv->BeginScope();
  auto root_ty = this->root_->SemAnalyze(venv, tenv, 0, errormsg);
  // venv->EndScope();
  // TODO the root expression?
}

type::Ty *SimpleVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  auto entry = venv->Look(this->sym_);
  if (!(entry && is_var_entry(entry))) {
    errormsg->Error(this->pos_, "undefined variable %s",
                    this->sym_->Name().data());
    return type::IntTy::Instance();
  }
  return static_cast<env::VarEntry *>(entry)->ty_->ActualTy();
}

type::Ty *FieldVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  auto val_ty = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  assert(val_ty); // TODO
  if (!is_record_type(val_ty)) {
    errormsg->Error(this->pos_, "not a record type");
    return type::IntTy::Instance();
  }
  auto field_list = static_cast<type::RecordTy *>(val_ty)->fields_->GetList();
  auto found_field_it = std::find_if(
      field_list.begin(), field_list.end(), [this](type::Field *field) -> bool {
        return field->name_->Name() == this->sym_->Name();
      });
  if (found_field_it == field_list.end()) {
    errormsg->Error(this->pos_, "field %s doesn't exist",
                    this->sym_->Name().c_str());
    return type::IntTy::Instance();
  }
  return (*found_field_it)->ty_->ActualTy();
}

type::Ty *SubscriptVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   int labelcount,
                                   err::ErrorMsg *errormsg) const {
  auto array_ty = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  assert(array_ty);
  if (!is_array_type(array_ty)) {
    errormsg->Error(this->pos_, "array type required");
    return type::IntTy::Instance();
  }
  auto index_ty =
      this->subscript_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!is_int_type(index_ty)) {
    errormsg->Error(this->pos_, "integer required");
  }
  return static_cast<type::ArrayTy *>(array_ty)->ty_->ActualTy();
}

type::Ty *VarExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);
}

type::Ty *NilExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return type::NilTy::Instance();
}

type::Ty *IntExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return type::IntTy::Instance();
}

type::Ty *StringExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  return type::StringTy::Instance();
}

type::Ty *CallExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  auto fun_entry = venv->Look(this->func_);
  if (!(fun_entry && is_fun_entry(fun_entry))) {
    errormsg->Error(this->pos_, "undefined function %s",
                    this->func_->Name().data());
    return type::VoidTy::Instance();
  }
  // Check arguments
  {
    auto arg_list = this->args_->GetList();
    auto formals = static_cast<env::FunEntry *>(fun_entry)->formals_->GetList();
    auto arg_it = arg_list.begin();
    auto formal_it = formals.begin();

    while (arg_it != arg_list.end() && formal_it != formals.end()) {
      auto expected_ty = (*formal_it)->ActualTy();
      if (!expected_ty) {
        // TODO
        continue;
      }
      auto arg_ty = (*arg_it)->SemAnalyze(venv, tenv, labelcount, errormsg);

      if (!is_same_d_type(arg_ty, expected_ty)) {
        errormsg->Error(this->pos_, "para type mismatch");
      }

      arg_it++;
      formal_it++;
    }

    if (arg_it != arg_list.end()) {
      errormsg->Error(this->pos_, "too many params in function %s",
                      this->func_->Name().data());
    }
    if (formal_it != formals.end()) {
      // TODO
    }
  }
  // Return the returned type
  return static_cast<env::FunEntry *>(fun_entry)->result_;
}

type::Ty *OpExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  auto lhs_ty = this->left_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto rhs_ty = this->right_->SemAnalyze(venv, tenv, labelcount, errormsg);
  switch (this->oper_) {
  // Comparison
  // Equal and no-equal: also on the record/arr type, compare by "reference"
  case Oper::EQ_OP:
  case Oper::NEQ_OP:
  case Oper::LE_OP:
  case Oper::GE_OP:
  case Oper::LT_OP:
  case Oper::GT_OP:
    if (!is_same_d_type(lhs_ty, rhs_ty)) {
      errormsg->Error(this->pos_, "same type required");
    }
    break;
  case Oper::PLUS_OP:
  case Oper::MINUS_OP:
  case Oper::TIMES_OP:
  case Oper::DIVIDE_OP:
    if (!(is_int_type(lhs_ty) && is_int_type(rhs_ty))) {
      errormsg->Error(this->pos_, "integer required");
    }
  default:
    break;
  }
  /* TODO: Put your lab4 code here */
  return type::IntTy::Instance();
}

type::Ty *RecordExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  auto entry = tenv->Look(this->typ_);
  if (!entry) {
    errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
    return type::VoidTy::Instance();
  }
  if (!is_record_type(entry->ActualTy())) {
    // TODO
  }
  auto matched_ty = static_cast<type::RecordTy *>(entry)->ActualTy();
  // TODO check fields?
  return matched_ty;
}

type::Ty *SeqExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  // During parsing, if the SeqExp is empty, I construct a VoidExp.
  type::Ty *ret_ty = nullptr;
  assert(!this->seq_->GetList().empty());
  for (auto exp : this->seq_->GetList()) {
    ret_ty = exp->SemAnalyze(venv, tenv, labelcount, errormsg);
  }
  assert(ret_ty);
  return ret_ty;
}

type::Ty *AssignExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  // o.O Unexpected coupled?
  if (typeid(*this->var_) == typeid(absyn::SimpleVar)) {
    auto var_entry =
        venv->Look(static_cast<absyn::SimpleVar *>(this->var_)->sym_);
    if (var_entry && var_entry->readonly_) {
      // Fortunately, Tiger only has one kind of immutable variables.
      errormsg->Error(this->pos_, "loop variable can't be assigned");
    }
  }
  auto lvalue_ty = this->var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  assert(lvalue_ty); // TODO
  auto exp_ty = this->exp_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!is_same_d_type(lvalue_ty, exp_ty)) {
    errormsg->Error(this->pos_, "unmatched assign exp");
  }
  return type::VoidTy::Instance();
}

type::Ty *IfExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  auto test_ty = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*test_ty) != typeid(type::IntTy)) {
    // TODO test type error
  }
  assert(this->then_);
  auto exp1_ty = this->then_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (this->elsee_ == nullptr) {
    // If-then mst produces no value
    if (!is_void_type(exp1_ty)) {
      errormsg->Error(this->pos_, "if-then exp's body must produce no value");
    }
    return type::VoidTy::Instance();
  }

  auto exp2_ty = this->elsee_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!is_same_d_type(exp1_ty, exp2_ty)) {
    errormsg->Error(this->pos_, "then exp and else exp type mismatch");
    return type::IntTy::Instance();
  }
  return exp1_ty;
}

type::Ty *WhileExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  auto test_ty = this->test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*test_ty) != typeid(type::IntTy)) {
    // TODO test type error
  }
  auto exp_ty = this->body_->SemAnalyze(venv, tenv, labelcount + 1, errormsg);
  if (typeid(*exp_ty) != typeid(type::VoidTy)) {
    errormsg->Error(this->pos_, "while body must produce no value");
    return type::VoidTy::Instance();
  }
  return type::VoidTy::Instance();
}

type::Ty *ForExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  auto hi_ty = this->hi_->SemAnalyze(venv, tenv, labelcount, errormsg);
  auto lo_ty = this->lo_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!(is_int_type(hi_ty) && is_int_type(lo_ty))) {
    errormsg->Error(this->pos_, "for exp's range type is not integer");
  }
  venv->BeginScope();
  venv->Enter(this->var_, new env::VarEntry(type::IntTy::Instance(), true));
  auto body_ty = this->body_->SemAnalyze(venv, tenv, labelcount + 1, errormsg);
  venv->EndScope();
  if (!is_void_type(body_ty)) {
  }
  return type::VoidTy::Instance();
}

type::Ty *BreakExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  if (labelcount == 0) {
    errormsg->Error(this->pos_, "break is not inside any loop");
  }
  return type::VoidTy::Instance();
}

type::Ty *LetExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();
  for (auto dec : this->decs_->GetList()) {
    dec->SemAnalyze(venv, tenv, labelcount, errormsg);
  }
  auto ret_ty = this->body_->SemAnalyze(venv, tenv, labelcount, errormsg);
  venv->EndScope();
  tenv->EndScope();
  return ret_ty;
}

type::Ty *ArrayExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  auto entry = tenv->Look(this->typ_);
  if (!entry) {
    errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
    return type::VoidTy::Instance();
  }
  if (!is_array_type(entry->ActualTy())) {
    errormsg->Error(this->pos_, "array type required");
    return type::VoidTy::Instance();
  }

  auto array_ty = static_cast<type::ArrayTy *>(entry->ActualTy());
  auto expected_ty = array_ty->ty_;

  auto size_ty = this->size_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!is_int_type(size_ty)) {
    // TODO
    // error message
    return type::VoidTy::Instance();
  }
  auto init_ty = this->init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!is_same_d_type(init_ty, expected_ty)) {
    errormsg->Error(this->pos_, "type mismatch");
    return type::VoidTy::Instance();
  }
  return array_ty;
}

type::Ty *VoidExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  /* TODO: Put your lab4 code here */
  return type::VoidTy::Instance();
}

void FunctionDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  auto a_fun_list = this->functions_->GetList();
  /**
   * @brief Since env::FunEntry doesn't store the symbol name of the function,
   *        we need to store the symbol (fun->name_) alongside.
   *
   * Use std::string_view as the key:
   *      1. STL provides the hash function for std::string_view
   *      2. std::string can be converted to std::string_view implicitly
   *      3. I'm sure lifetime of all the strings (sym::Symbol) covers the map
   */
  std::unordered_map<std::string_view, env::FunEntry *> s_fun_map{};

  // Enter the "headers"
  for (auto fun : a_fun_list) {
    // Check duplicate.
    if (s_fun_map.find(fun->name_->Name()) != s_fun_map.end()) {
      errormsg->Error(this->pos_, "two functions have the same name");
      return;
    }
    // analyze the formals and result
    auto formals = fun->params_->MakeFormalTyList(tenv, errormsg);
    // If fun->result == nullptr, it declare a **procedure**.
    auto result =
        fun->result_ ? tenv->Look(fun->result_) : type::VoidTy::Instance();
    if (!result) {
      errormsg->Error(this->pos_, "undefined type %s",
                      fun->result_->Name().c_str());
    }
    auto fun_entry = new env::FunEntry{formals, result};
    // Leave the function body untouched.
    venv->Enter(fun->name_, fun_entry);
    s_fun_map.insert({fun->name_->Name(), fun_entry});
  }

  // Check the function body
  for (auto fun : a_fun_list) {
#ifdef NDEBUG
    auto fun_entry = s_fun_map.find(fun->name_->Name())->second;
#else
    auto it = s_fun_map.find(fun->name_->Name());
    assert(it != s_fun_map.end());
    auto fun_entry = it->second;
#endif
    venv->BeginScope();
    {
      auto param_list = fun->params_->GetList();
      auto formals = fun_entry->formals_->GetList();
      assert(param_list.size() == formals.size());

      auto param_it = param_list.begin();
      auto formal_it = formals.begin();

      while (param_it != param_list.end()) {
        venv->Enter((*param_it)->name_,
                    new env::VarEntry((*formal_it)->ActualTy()));
        param_it++;
        formal_it++;
      }
      assert(formal_it == formals.end());
    }

    auto ret_ty = fun->body_->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (!is_same_d_type(ret_ty, fun_entry->result_)) {
      if (is_void_type(fun_entry->result_)) {
        errormsg->Error(this->pos_, "procedure returns value");
      }
      // TODO
    }
    venv->EndScope();
  }
}

void VarDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                        err::ErrorMsg *errormsg) const {
  auto ty_entry = this->typ_ ? tenv->Look(this->typ_) : nullptr;
  if (this->typ_ && !ty_entry) {
    errormsg->Error(this->pos_, "undefined type %s", this->typ_->Name().data());
  }
  auto init_ty = this->init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (this->typ_ && !is_same_d_type(ty_entry, init_ty)) {
    errormsg->Error(this->pos_, "type mismatch");
  }
  if (!this->typ_ && is_nil_type(init_ty)) {
    errormsg->Error(this->pos_,
                    "init should not be nil without type specified");
  }
  venv->Enter(var_, new env::VarEntry(init_ty));
}

void TypeDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                         err::ErrorMsg *errormsg) const {
  auto a_named_type_list = this->types_->GetList();
  std::vector<type::NameTy *> s_named_type_list{};
  // Should be a DAG
  DirectGraph dg{a_named_type_list.size()};
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
      return;
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

      if (is_name_type(actual_ty)) {

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
}

type::Ty *NameTy::SemAnalyze(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
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

type::Ty *RecordTy::SemAnalyze(env::TEnvPtr tenv,
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

type::Ty *ArrayTy::SemAnalyze(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  auto entry = tenv->Look(this->array_);
  if (!entry) {
    errormsg->Error(this->pos_, "undefined type %s",
                    this->array_->Name().data());
    return new type::ArrayTy{type::IntTy::Instance()};
  }
  return new type::ArrayTy{entry->ActualTy()};
}

} // namespace absyn

namespace sem {

void ProgSem::SemAnalyze() {
  FillBaseVEnv();
  FillBaseTEnv();
  absyn_tree_->SemAnalyze(venv_.get(), tenv_.get(), errormsg_.get());
}

} // namespace tr
