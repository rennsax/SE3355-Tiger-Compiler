#ifndef TIGER_FRAME_TEMP_H_
#define TIGER_FRAME_TEMP_H_

#include "tiger/symbol/symbol.h"

#include <list>

namespace temp {

using Label = sym::Symbol;

class LabelFactory {
public:
  /**
   * @brief Returns a new label from an infinite set of labels.
   *
   */
  static Label *NewLabel();
  /**
   * @brief Returns a new label whose assembly-language name is string.
   *
   * @param name it's OK to use the symbol name (e.g. the function name) to make
   * your debug life easier. However, there may be different functions with the
   * identical name in different scopes. (P142)
   */
  static Label *NamedLabel(std::string_view name);
  static std::string LabelString(Label *s);

private:
  int label_id_ = 0;
  static LabelFactory label_factory;
};

/**
 * @brief Virtual register.
 *
 */
class Temp {
  friend class TempFactory;

public:
  [[nodiscard]] int Int() const;

private:
  int num_;
  explicit Temp(int num) : num_(num) {}
};

class TempFactory {
public:
  /**
   * @brief Returns a new temporary from an infinite set of temps.
   *
   * We've assumed that the registers are infinite.
   */
  static Temp *NewTemp();

private:
  int temp_id_ = 100;
  static TempFactory temp_factory;
};

/**
 * @brief Temp mapping.
 *
 * A table whose keys are temp::Temp and whose bindings are @c std::string s.
 *
 * (P207) The map is primarily used by the register allocator. But it's
 * convenient of the @c Frame module for describing the names of each
 * preallocated registers (before register allocation, during codegen).
 *
 */
class Map {
public:
  void Enter(Temp *t, std::string *s);
  std::string *Look(Temp *t);
  void DumpMap(FILE *out);

  /// Create a new, empty map.
  static Map *Empty();
  static Map *Name();
  static Map *LayerMap(Map *over, Map *under);

private:
  tab::Table<Temp, std::string> *tab_;
  Map *under_;

  Map() : tab_(new tab::Table<Temp, std::string>()), under_(nullptr) {}
  Map(tab::Table<Temp, std::string> *tab, Map *under)
      : tab_(tab), under_(under) {}
};

class TempList {
public:
  explicit TempList(Temp *t) : temp_list_({t}) {}
  TempList(std::initializer_list<Temp *> list) : temp_list_(list) {}
  TempList() = default;
  void Append(Temp *t) { temp_list_.push_back(t); }
  [[nodiscard]] Temp *NthTemp(int i) const;
  [[nodiscard]] const std::list<Temp *> &GetList() const { return temp_list_; }

private:
  std::list<Temp *> temp_list_;
};

} // namespace temp

#endif