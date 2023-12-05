#ifndef TIGER_TRANSLATE_TRANSLATE_H_
#define TIGER_TRANSLATE_TRANSLATE_H_

#include <list>
#include <memory>

#include "tiger/absyn/absyn.h"
#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"
#include "tiger/semant/types.h"

namespace tr {

/**
 * @brief All visible abstraction declared in namespace tr.
 *
 * To decouple each module, the derived types of Exp aren't declared here.
 * The interfaces of Exp are also omitted.
 *
 */
class Exp;       // Defined in translate.cc
struct ExpAndTy; // Defined in translate.cc
struct Access;   // Defined below
class Level;     // Defined below
class ProgTr;    // Defined below

/**
 * @brief Interface between Translate and Semant.
 *
 * The textbook only specifies an example:
 *        Tr_Exp Tr_simpleVar(Tr_Access, Tr_Level);
 *
 * (P158) In fact, this is a good rule of thumb in determining the interface
 * between Semant and Translate: the Semant module should not contain any direct
 * reference to the Tree or Frame module. Any manipulation of IR trees should be
 * done by Translate.
 * (P160) The rule of thumb just given applies to all of them; there should be a
 * Translate function to handle array subscripts, one for record fields, one for
 * each kind of expression, and so on.
 *
 * Semant can pass the `access` of x (obtained from tr::Level::allocLocal) and
 * the `level` of the function in which x is used and get back a tr::Exp.
 * Therefore, Semant will never know about the tree language, and not contain
 * any direct reference to the Tree or Frame module.
 *
 */

/**
 * @brief
 *
 * MEM(+(TEMP FP, CONST k))
 *
 * @param access tr::Access
 * @param level tr::Level
 * @return Exp*
 */
tr::Exp *makeSimpleVariable(tr::Access *access, tr::Level *level);

void procEntryExit(tr::Level *level, tr::Exp *body,
                   const std::list<tr::Access *> &accessList);

/**
 * @brief Represents "a list of places where a label must be filled in"
 *
 * In essence, it contains a list of pointers to temp::Label* (temp::Label**).
 * After PatchList::DoPatch(temp::Label* l) is called, each temp::Label* in the
 * patch list is assigned to l.
 *
 * The data structure is used in tr::Cx and tr::CxExp.
 *
 */
class PatchList {
public:
  /**
   * @brief Do the patch.
   *
   * @param label which label should be filled in.
   */
  void DoPatch(temp::Label *label) {
    for (auto &patch : patch_list_)
      *patch = label;
  }

  /**
   * @brief A factory method to join two path lists together.
   *
   * @param first
   * @param second
   * @return PatchList
   */
  static PatchList JoinPatch(const PatchList &first, const PatchList &second) {
    // Copy-construct from the first one.
    PatchList ret(first.GetList());
    // Push the elements to the end one by one.
    for (auto &patch : second.patch_list_) {
      ret.patch_list_.push_back(patch);
    }
    return ret;
  }

  explicit PatchList(std::list<temp::Label **> patch_list)
      : patch_list_(patch_list) {}
  PatchList() = default;

  [[nodiscard]] const std::list<temp::Label **> &GetList() const {
    return patch_list_;
  }

private:
  std::list<temp::Label **> patch_list_;
};

/**
 * @brief new version of Access, used in translation phase.
 *
 * Compared with frame::Access:
 * tr::Access is an abstract data type. It must know about static link.
 */
struct Access {
  Level *level_;
  frame::Access *access_;

  Access(Level *level, frame::Access *access)
      : level_(level), access_(access) {}
};

/**
 * @brief The nesting level
 *
 */
class Level {
public:
  /* TODO: Put your lab5 code here */
  /**
   * @brief Create a nesting level.
   *
   * Aside: managing **static link** (P144)
   * The management of static link is passing a extra parameter "static link" to
   * the nesting function / frame / level. The param "static link" is just a
   * pointer to the linked frame.
   * Static link should be stored in the frame, at the part of "incoming
   * arguments":
   * ------------ incoming arguments
   * argument n
   * argument n-1
   * ...
   * argument 1
   * static link
   * ------------
   * We implement static link by passing an extra parameter that is always
   * escaped. For instance, if we have a function g, whose corresponded level is
   * level_g (tr::Level), and function f(x, y) is nested inside g. We can call
   *              level_g.newLevel(f, {false, false});
   * to create the abstract level level_f (tr::Level) of function f, whose two
   * actual parameters are both not escaped. Meanwhile, the procedure above
   * (tr::Level::newLevel(label, fmls)) may call:
   *              frame::newFrame(label, {true, fmls});
   * which returns a frame::Frame containing three frame-offset values - the
   * static link offset and the offset of x and y.
   *
   * @param this the parent level
   * @param name
   * @param formals whether the formals are escaped
   */
  [[nodiscard]] tr::Level *newLevel(temp::Label *name,
                                    const std::list<bool> &formals) const;

  /**
   * @brief Get the actual formals of the function.
   *
   * It will get offsets in the frame (frame::Frame), strip the static link one,
   * and suitably converted into Access values.
   */
  [[nodiscard]] const std::list<Access *> &formals() const;

  /**
   * @brief Create a local variable in the level.
   *
   * When Semant processes a local variable declaration at level lev, it calls
   * lev->allocLocal(esc) to create the variable in this level. (P143)
   *
   * @param escape whether the variable is escaped
   * @return Access*
   */
  [[nodiscard]] Access *allocLocal(bool escape) const;


private:
  /**
   * @brief The constructor is only called when creating the "main" level.
   * All nesting levels should be created by calling Level::newLevel.
   *
   */
  Level() = default;

  frame::Frame *frame_;
  Level *parent_;

};

class ProgTr {
public:
  // TODO: Put your lab5 code here */
  ProgTr(std::unique_ptr<absyn::AbsynTree> ast,
         std::unique_ptr<err::ErrorMsg> error)
      : absyn_tree_{std::move(ast)}, errormsg_{std::move(error)} {}

  /**
   * Translate IR tree
   */
  void Translate();

  /**
   * Transfer the ownership of errormsg to outer scope
   * @return unique pointer to errormsg
   */
  std::unique_ptr<err::ErrorMsg> TransferErrormsg() {
    return std::move(errormsg_);
  }

private:
  std::unique_ptr<absyn::AbsynTree> absyn_tree_;
  std::unique_ptr<err::ErrorMsg> errormsg_;
  // It is the level within which that program is nested.
  // All library functions are declared at this level.
  std::unique_ptr<tr::Level> main_level_;

  std::unique_ptr<env::TEnv> tenv_;
  std::unique_ptr<env::VEnv> venv_;

  // Fill base symbol for var env and type env
  void FillBaseVEnv();
  void FillBaseTEnv();
};

} // namespace tr

#endif
