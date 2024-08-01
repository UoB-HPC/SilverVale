#include <atomic>
#include <fstream>
#include <iostream>
#include <string>
#include <thread>
#include <utility>
#include <variant>

#include "sv/database.h"
#include "sv/exec.h"
#include "sv/tree.h"

#include "gcc-plugin.h"

#include "context.h"
#include "plugin-version.h"
#include "tree.h"

#include "gimple.h"
#include "internal-fn.h"
#include "tree-pass.h"

#include "gimple-iterator.h"
#include "gimple-pretty-print.h"

#include "opts.h"

#include "aspartame/optional.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "fmt/core.h"

using namespace aspartame;
using namespace std::string_literals;

template <typename Node, typename... Fs>
void visitDyn0(Node n, Fs... fs) {           // NOLINT(*-no-recursion)
  [[maybe_unused]] auto _ = ([&]() -> bool { // NOLINT(*-no-recursion)
    if constexpr (std::is_same_v<sv::arg0_t<Fs>, Node>) {
      fs(n);
      return true;
    } else if (auto x = safe_dyn_cast<sv::arg0_t<Fs>>(n)) {
      fs(x);
      return true;
    }
    return false;
  }() || ...);
}

using RenameFn = std::function<std::string(const std::string &)>;

template <typename T> using R = std::shared_ptr<T>;
template <typename... T> using Alternatives = std::variant<R<T>...>;
template <typename T, typename... Args> static std::shared_ptr<T> r(Args &&...args) {
  return std::make_shared<T>(std::forward<Args>(args)...);
}

struct Set;
struct Cond;
struct Call;
struct Select;
struct Ref;
struct MemRef;
struct Verbatim;
using LHS = Alternatives<Select, MemRef, Ref, Verbatim>;
using Any = Alternatives<Set, Cond, Call, Select, Ref, MemRef, Verbatim>;

struct Name {
  std::string name{};
};

struct Cond {
  Name lhs;
};

struct Verbatim {
  std::string value;
};

struct Ref {
  Name base;
  static Ref illegal(const std::string &name) {
    return {Name(fmt::format("UNEXPECTED ILLEGAL<{}>", name))};
  }
};

struct MemRef { // https://gcc.gnu.org/onlinedocs/gccint/Storage-References.html
  LHS base;
  std::optional<LHS> offset{}, idx{}, step{}, idx2{};
};

struct Select {
  std::vector<LHS> chain;
};

struct Call {
  LHS lhs;
};

struct Set {
  LHS lhs;
  std::string op;
};

struct Node {

  Any kind;
  std::string comment{};
  sv::Location location;

  [[nodiscard]] static std::string to_string(const LHS &lhs,           // NOLINT(*-no-recursion)
                                             const RenameFn &f) {      // NOLINT(*-no-recursion)
    return std::visit([&](auto &&x) { return to_string(x, f); }, lhs); // NOLINT(*-no-recursion)
  }
  [[nodiscard]] static std::string to_string(const Any &kind, const RenameFn &f) {
    return std::visit([&](auto &&x) { return to_string(x, f); }, kind);
  }

  [[nodiscard]] static std::string to_string(const R<Cond> &x, const RenameFn &f) {
    return fmt::format("COND {}", f(x->lhs.name));
  }
  [[nodiscard]] static std::string to_string(const R<Verbatim> &x, const RenameFn &) {
    return x->value;
  }
  [[nodiscard]] static std::string to_string(const R<Ref> &x, const RenameFn &f) {
    return fmt::format("%{}", f(x->base.name));
  }
  [[nodiscard]] static std::string to_string(const R<MemRef> &x,  // NOLINT(*-no-recursion)
                                             const RenameFn &f) { // NOLINT(*-no-recursion)
    auto show = [&](auto &maybe, auto prefix) {                   // NOLINT(*-no-recursion)
      return maybe ^
             fold([&](auto r) { return prefix + to_string(r, f); }, // NOLINT(*-no-recursion)
                  []() { return ""s; });                            // NOLINT(*-no-recursion)
    };
    return fmt::format("{}[{}{}{}]", //
                       to_string(x->base, f), show(x->offset, "+"), show(x->idx, "+"),
                       show(x->step, "*"), show(x->idx2, "+"));
  }

  [[nodiscard]] static std::string to_string(const R<Select> &x,  // NOLINT(*-no-recursion)
                                             const RenameFn &f) { // NOLINT(*-no-recursion)
    return x->chain |
           mk_string("->", [&](auto &p) { return to_string(p, f); }); // NOLINT(*-no-recursion)
  }
  [[nodiscard]] static std::string to_string(const R<Call> &x, const RenameFn &f) {
    return fmt::format("{} = CALL", to_string(x->lhs, f));
  }
  [[nodiscard]] static std::string to_string(const R<Set> &x, const RenameFn &f) {
    return fmt::format("{} = OP<{}>", to_string(x->lhs, f), x->op);
  };

  [[nodiscard]] std::string to_string(const RenameFn &f, bool comments = true) const {
    return fmt::format("{} {}", to_string(kind, f),
                       (!comments || comment.empty() ? "" : "# " + comment));
  };
};

[[nodiscard]] static sv::Location resolveLocation(location_t loc) {
  if (loc == UNKNOWN_LOCATION) return {};
  auto el = expand_location(loc);
  return {.filename = el.file ? el.file : "",
          .line = static_cast<size_t>(el.line),
          .col = static_cast<size_t>(el.column)};
}

[[nodiscard]] static sv::Location resolveLocation(tree node) {
  if (DECL_P(node)) {
    return resolveLocation(DECL_SOURCE_LOCATION(node));
  } else if (EXPR_P(node)) {
    return resolveLocation(EXPR_LOCATION(node));
  } else {
    return resolveLocation(UNKNOWN_LOCATION);
  }
}

[[nodiscard]] static sv::Location resolveLocation(gimple *gimple) {
  if (auto gloc = gimple_location(gimple); gloc != UNKNOWN_LOCATION) {
    auto el = expand_location(gloc);
    return {.filename = el.file ? el.file : "",
            .line = static_cast<size_t>(el.line),
            .col = static_cast<size_t>(el.column)};
  }
  return {};
}

class GimpleUprootPass : public gimple_opt_pass, sv::NTreeVisitor<Node, void> {
  static constexpr pass_data data = {
      GIMPLE_PASS,        /* type */
      "GimpleUprootPass", /* name */
      OPTGROUP_NONE,      /* optinfo_flags */
      TV_NONE,            /* tv_id */
      0,                  /* properties_required */
      0,                  /* properties_provided */
      0,                  /* properties_destroyed */
      0,                  /* todo_flags_start */
      0                   /* todo_flags_finish */
  };
  std::atomic_long nameCounter{};
  sv::NTree<Node> root;
  std::string basename, afterPass, kind;

public:
  explicit GimpleUprootPass(gcc::context *ctx, std::string basename, std::string afterPass,
                            std::string kind)
      : gimple_opt_pass(data, ctx), sv::NTreeVisitor<Node, void>(&root),
        basename(std::move(basename)), afterPass(std::move(afterPass)), kind(std::move(kind)) {
    root.value = Node{r<Ref>(Name{this->basename}), {}, sv::Location{}};
  }

  static std::string to_string(tree tree) { return print_generic_expr_to_str(tree); }

  static LHS reprLHSStrict(tree tree) { // NOLINT(*-no-recursion)
    if (auto lhs = reprLHS(tree); lhs) {
      return *lhs;
    } else {
      return r<Ref>(Ref::illegal(
          fmt::format("ILLEGAL[{}]<{}>", get_tree_code_name(TREE_CODE(tree)), to_string(tree))));
    }
  }

  static std::optional<LHS> reprLHS(tree tree) { // NOLINT(*-no-recursion)
    if (!tree) { return r<Verbatim>("<NULL>"); }
    if (is_gimple_constant(tree)) return r<Verbatim>(fmt::format("^{}", to_string(tree)));
    switch (auto code = TREE_CODE(tree); code) {
      case ADDR_EXPR: return r<Ref>(Name{fmt::format("@{}", to_string(tree))});
      case RESULT_DECL: [[fallthrough]]; // TODO not entirely sure about this one, exists only once?
      case SSA_NAME: [[fallthrough]];
      case PARM_DECL: [[fallthrough]];
      case VAR_DECL: [[fallthrough]];
      case FIELD_DECL:
        return r<Ref>(Name{fmt::format("{}({})", to_string(tree), get_tree_code_name(code))});
      case TARGET_MEM_REF: {
        auto base = TREE_OPERAND(tree, 0), offset = TREE_OPERAND(tree, 1),
             idx = TREE_OPERAND(tree, 1), step = TREE_OPERAND(tree, 1),
             idx2 = TREE_OPERAND(tree, 1);
        auto arg = [&](::tree t) { // NOLINT(*-no-recursion)
          return t ? std::optional{reprLHSStrict(t)} : std::nullopt;
        };
        return r<MemRef>(reprLHSStrict(base), arg(offset), arg(idx), arg(step), arg(idx2));
      }
      case ARRAY_REF: {
        auto base = TREE_OPERAND(tree, 0), index = TREE_OPERAND(tree, 1);
        return r<MemRef>(reprLHSStrict(base), std::nullopt,
                         index ? std::optional{reprLHSStrict(index)} : std::nullopt);
      }
      case MEM_REF: {
        auto base = TREE_OPERAND(tree, 0), offset = TREE_OPERAND(tree, 1);
        return r<MemRef>(reprLHSStrict(base),
                         offset ? std::optional{reprLHSStrict(offset)} : std::nullopt);
      }
      case COMPONENT_REF: { // this is a NULL_TREE terminated list of selects
        auto select = r<Select>();
        for (int i = 0; i < TREE_OPERAND_LENGTH(tree); ++i) {
          if (auto path = TREE_OPERAND(tree, i); path) {
            select->chain.emplace_back(reprLHS(path).value_or(r<Ref>(Ref::illegal(fmt::format(
                "Chain: {}({})", to_string(tree), get_tree_code_name(TREE_CODE(tree)))))));
          }
        }
        return select;
      }
      default: return {};
    }
  }

  Name mkName() { return {fmt::format("_uproot_syth_{}_", nameCounter++)}; }

  void repr(internal_fn fn) {
    single(Node{r<Verbatim>(fmt::format("#{}", internal_fn_name(fn))), {}, resolveLocation(fn)});
  }

  void repr(tree tree, const std::string &comment = "") { // NOLINT(*-no-recursion)
    if (!tree) {
      single(Node{r<Verbatim>("<NULL>"), comment, sv::Location{}});
      return;
    }
    auto loc = resolveLocation(tree);
    if (auto lhs = reprLHS(tree); lhs) {
      single(std::visit([&](auto &&x) { return Node{x, comment, loc}; }, *lhs));
      return;
    }

    switch (auto code = TREE_CODE(tree); code) {
      case ARRAY_REF:
        single(
            Node{r<Set>(reprLHSStrict(TREE_OPERAND(tree, 0)), to_string((TREE_OPERAND(tree, 1)))),
                 fmt::format("array {}", comment), loc});
        break;
      case CONSTRUCTOR: single(Node{r<Verbatim>("Ctor"), comment, loc}); break;
      case OBJ_TYPE_REF:
        scoped(
            [&]() {
              auto lhs = TREE_OPERAND(tree, 1), offset = TREE_OPERAND(tree, 2);
              single(Node{
                  r<Select>(std::vector<LHS>{r<MemRef>(reprLHSStrict(lhs), reprLHSStrict(offset))}),
                  "type ref", loc});
            },
            Node{r<Call>(r<Ref>(Name{to_string(TREE_OPERAND(tree, 0))})), comment, loc});
        break;

        // the following three appear as gimple_assign <*_expr, ?, *OP <$0>, {}, {}>
        // so we simply discard the EXPR
      case REALPART_EXPR: repr(TREE_OPERAND(tree, 0), "realpart"); break;
      case IMAGPART_EXPR: repr(TREE_OPERAND(tree, 0), "imagpart"); break;
      case VIEW_CONVERT_EXPR: repr(TREE_OPERAND(tree, 0), "view_convert"); break;
      case BIT_FIELD_REF:
        scoped(
            [&]() { // NOLINT(*-no-recursion)
              repr(TREE_OPERAND(tree, 1));
              repr(TREE_OPERAND(tree, 2));
            },
            std::visit([&](auto &&x) { return Node{x, fmt::format("bitfield {}", comment), loc}; },
                       reprLHSStrict(TREE_OPERAND(tree, 0))));
        break;
      default: {
        auto desc =
            fmt::format("UNEXPECTED {}: {} ({} x{})", to_string(tree), get_tree_code_name(code),
                        TREE_CODE_CLASS_STRING(TREE_CODE_CLASS(code)), TREE_OPERAND_LENGTH(tree));
        if (auto nOperands = TREE_OPERAND_LENGTH(tree); nOperands == 0)
          single(Node{r<Verbatim>(desc), comment, loc});
        else
          scoped(
              [&]() { // NOLINT(*-no-recursion)
                for (int i = 0; i < nOperands; ++i)
                  repr(TREE_OPERAND(tree, i));
              },
              Node{r<Verbatim>(desc), comment, loc});

      } break;
    };
  }

  void repr(gimple *expr, const std::string &comment = "") { // NOLINT(*-no-recursion)
    if (!expr) {
      single(Node{r<Verbatim>("<NULL>"), comment, sv::Location{}});
      return;
    }

    auto loc = resolveLocation(expr);

    visitDyn0(
        expr, //
        [&](gdebug *debug) { /*discard*/ },
        [&](gbind *bind) { // NOLINT(*-no-recursion)
          repr(gimple_bind_vars(bind));
          //          repr(gimple_bind_block(bind));
          repr(gimple_bind_body(bind));
        },
        [&](gtry *gtry) { // NOLINT(*-no-recursion)
          scoped(
              [&]() { // NOLINT(*-no-recursion)
                repr(gimple_try_eval(gtry));
                auto cleanup = gimple_try_cleanup(gtry);
                scoped(
                    [&]() { // NOLINT(*-no-recursion)
                      repr(cleanup);
                    },
                    Node{r<Verbatim>("FINALLY"), {}, resolveLocation(cleanup)});
              },
              Node{r<Verbatim>("TRY"), "", loc});
        },
        [&](gphi *expr) {
          if (virtual_operand_p(gimple_phi_result(expr))) return;
          scoped(
              [&]() {
                for (size_t i = 0; i < gimple_phi_num_args(expr); ++i) {

                  repr(gimple_phi_arg_def(expr, i),
                       std::string(
                           fmt::format("from L{}", gimple_phi_arg_edge(expr, i)->src->index)));
                }
              },
              Node{r<Set>(reprLHSStrict(gimple_phi_result(expr)), fmt::format("phi {}", comment)),
                   {},
                   loc});
        },
        [&](ggoto *expr) {
          single(Node{r<Verbatim>(fmt::format("GOTO {}", to_string((gimple_goto_dest(expr))))),
                      comment, loc});
        },
        [&](glabel *expr) {
          single(Node{r<Verbatim>(fmt::format("{} = LABEL", to_string((gimple_label_label(expr))))),
                      comment, loc});
        },
        [&](gcond *expr) {
          auto name = mkName();
          scoped(
              [&]() {
                repr(gimple_cond_lhs(expr));
                repr(gimple_cond_rhs(expr));
              },
              Node{r<Set>(r<Ref>(name), get_tree_code_name(gimple_cond_code(expr))),
                   fmt::format("cond {}", comment), loc});
          // XXX keep the last node as cond, so we can string up the successors
          single(Node{r<Cond>(name), comment, loc});
        },
        [&](gcall *call) {
          scoped(
              [&]() {
                if (!gimple_call_internal_p(call)) repr(gimple_call_fn(call));
                else repr(gimple_call_internal_fn(call));
                for (size_t i = 0; i < gimple_call_num_args(call); ++i)
                  repr(gimple_call_arg(call, i));
              },
              Node{r<Call>(reprLHSStrict(gimple_call_lhs(call))), comment, loc});
        },
        [&](gassign *expr) {
          std::vector<tree> operands;

          switch (gimple_assign_rhs_class(expr)) {
            case GIMPLE_TERNARY_RHS:
              operands = {
                  gimple_assign_rhs1(expr),
                  gimple_assign_rhs2(expr),
                  gimple_assign_rhs3(expr),
              };
              break;
            case GIMPLE_BINARY_RHS:
              operands = {gimple_assign_rhs1(expr), gimple_assign_rhs2(expr)};
              break;
            case GIMPLE_UNARY_RHS: [[fallthrough]];
            case GIMPLE_SINGLE_RHS: operands = {gimple_assign_rhs1(expr)}; break;
            case GIMPLE_INVALID_RHS: [[fallthrough]];
            default: break;
          }
          scoped(
              [&]() {
                for (auto &op : operands)
                  repr(op);
              },
              Node{r<Set>(reprLHSStrict(gimple_assign_lhs(expr)),
                          get_tree_code_name(gimple_assign_rhs_code(expr))),
                   fmt::format("assign {}", comment), loc});
        },
        [&](greturn *expr) {
          scoped([&]() { repr(gimple_return_retval(expr)); },
                 Node{r<Verbatim>(fmt::format("RETURN")), comment, loc});
        },

        [&](gimple *expr) { // NOLINT(*-no-recursion)
          if (!gimple_seq_singleton_p(expr)) {
            for (auto gsi = gsi_start(expr); !gsi_end_p(gsi); gsi_next(&gsi))
              repr(gsi_stmt(gsi));
          } else {
            switch (expr->code) {
                //  case GIMPLE_ERROR_MARK: break;
                //  case GIMPLE_OMP_SECTIONS_SWITCH:break;
              case GIMPLE_PREDICT: [[fallthrough]];
              case GIMPLE_NOP: // ignore
                break;
              default:
                print_gimple_stmt(stderr, expr, 0, TDF_SLIM);
                single(Node{
                    r<Verbatim>(fmt::format("UNEXPECTED GIMPLE {}", gimple_code_name[expr->code])),
                    comment, loc});
            }
          }
        });
  }

  unsigned int execute(function *f) override {
    std::cout << "# [uproot] Witnessed: " << function_name(f) << std::endl;
    //  Keep a record of all the BB indices first #so we can know if we see a dead edge later

    if (f->cfg) { // low gimple after CFG contans BBs and not gimple_body
      std::unordered_set<int> bbIndices;
      basic_block bb;
      FOR_EACH_BB_FN(bb, f) { bbIndices.emplace(bb->index); }
      scoped(
          [&]() {
            basic_block bb;
            FOR_EACH_BB_FN(bb, f) {
              scoped(
                  [&]() {
                    for (auto pi = gsi_start_phis(bb); !gsi_end_p(pi); gsi_next(&pi))
                      repr(pi.phi());
                    for (auto gsi = gsi_start_bb(bb); !gsi_end_p(gsi); gsi_next(&gsi))
                      repr(gsi_stmt(gsi));
                    auto insertGoto = [&](auto &target) {
                      for (auto s : bb->succs) {
                        target.children.emplace_back(
                            bbIndices.contains(s->dest->index)
                                ? Node{r<Verbatim>(fmt::format("GOTO <L{}>", s->dest->index)),
                                       fmt::format(" terminal for L{}", bb->index), sv::Location{}}
                                : Node{r<Verbatim>(fmt::format("GOTO END <L{}>", s->dest->index)),
                                       fmt::format(" terminal for L{} to unknown", bb->index),
                                       sv::Location{}});
                      }
                    };
                    if (!node->children.empty()) {
                      auto &last = node->children.back();
                      insertGoto(std::holds_alternative<R<Cond>>(last.value.kind) ? last : *node);
                    } else {
                      for (auto s : bb->succs) {
                        node->children.emplace_back(
                            Node{r<Verbatim>(fmt::format("GOTO <L{}>", s->dest->index)),
                                 fmt::format(" terminal for L{}", bb->index), sv::Location{}});
                      }
                    }
                  },
                  Node{r<Verbatim>(fmt::format("BB <L{}>", bb->index)), {}, sv::Location{}});
            }
          },
          Node{r<Ref>(Name{function_name(f)}), {}, resolveLocation(f->decl)});
    } else {

      scoped([&]() { repr(f->gimple_body); },
             Node{r<Ref>(Name{function_name(f)}), {}, resolveLocation(f->decl)});
    }
    return 0;
  }

  static std::string to_lower(const std::string &xs) {
    std::string ys(xs.size(), '\0');
    std::transform(xs.begin(), xs.end(), ys.begin(),
                   [](auto c) { return TOLOWER(c); }); // GCC's nonsense
    return ys;
  }

  static bool hasEnv(const std::string &name) {
    if (auto valueCStr = std::getenv(name.c_str()); !valueCStr) {
      return false;
    } else {
      std::string value = to_lower(valueCStr);
      if (value == "1" || value == "true" || value == "on" || value == "yes") return true;
      return false;
    }
  }

  void flush() {
    size_t nodes{};
    root.postOrderWalk([&](auto, auto) { nodes++; });
    auto dump = [&](auto variant, auto f) {
      auto tree = root.map<sv::SNode>(f);
      if (hasEnv(fmt::format("UPROOT_SHOW_{}_{}", variant, kind))) tree.print(std::cout);
      auto pathEnv = fmt::format("UPROOT_{}_{}_PATH", variant, kind);
      if (auto path = std::getenv(pathEnv.c_str())) {
        std::ofstream out(path, std::ios::out);
        if (!out) std::cerr << "# [uproot] Unable to open " << path << " for writing" << std::endl;
        else {
          out << nlohmann::json(tree);
          std::cout << "# [uproot] Wrote " << nodes << " nodes to " << path << std::endl;
        }
      } else
        std::cout << "# [uproot] " << pathEnv << " not set, tree with " << nodes
                  << " nodes discarded" << std::endl;
    };
    dump("UNNAMED", [](auto &r) {
      return sv::SNode{r.to_string([](auto x) { return ""; }, false), r.location};
    });
    dump("NAMED", [](auto &r) {
      return sv::SNode{r.to_string([](auto x) { return x; }, true), r.location};
    });
  }
};

[[maybe_unused]] int plugin_is_GPL_compatible{};
[[maybe_unused]] int plugin_init(plugin_name_args *args, plugin_gcc_version *version) {
  std::cout << "# [uproot] Plugin loaded (name=" << args->base_name << ")" << std::endl;
  if (!plugin_default_version_check(version, &gcc_version)) {
    std::cerr << "[uproot] was compiled for  " << GCCPLUGIN_VERSION_MAJOR << "."
              << GCCPLUGIN_VERSION_MINOR << " but got " << version->basever << std::endl;
    return 1;
  }

  static plugin_info pluginInfo = {
      "0.0.1",
      "This plugins collects GIMPLE and prepares a JSON database for further processing ."};
  register_callback(args->base_name, PLUGIN_INFO, nullptr, &pluginInfo);
  register_callback(
      args->base_name, PLUGIN_START_UNIT,
      [](void *, void *data) {
        const auto name = reinterpret_cast<const char *>(data);
        auto basename = main_input_basename;
        std::cout << "# [uproot] Starting to uproot unit " << basename << std::endl;

        for (auto [afterPass, kind] : {//                 std::pair{"*warn_unused_result", "STREE"},
                                       std::pair{"optimized", "IRTREE"}}) {
          const auto pass = new GimpleUprootPass(g, basename, afterPass, kind);
          register_pass_info info{
              .pass = pass,
              .reference_pass_name = afterPass,
              .ref_pass_instance_number = 1,
              .pos_op = PASS_POS_INSERT_AFTER,
          };
          const auto flush = [](void *, void *data) {
            (reinterpret_cast<GimpleUprootPass *>(data))->flush();
          };
          register_callback(name, PLUGIN_PASS_MANAGER_SETUP, nullptr, &info);
          register_callback(name, PLUGIN_FINISH_UNIT, flush, pass);
        }
      },
      args->base_name);
  return 0;
}
