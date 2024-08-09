#include "tree_gimple.h"

#include "gcc-plugin.h"

#include "context.h"
#include "plugin-version.h"
#include "tree.h"

#include "gimple.h"
#include "internal-fn.h"

#include "gimple-iterator.h"
#include "gimple-pretty-print.h"

#include "aspartame/optional.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"
//
#include "sv/cli.h"

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

template <typename T, typename... Args> static std::shared_ptr<T> r(Args &&...args) {
  return std::make_shared<T>(std::forward<Args>(args)...);
}

sv::Ref sv::Ref::illegal(const std::string &name) {
  return {Name(fmt::format("UNEXPECTED ILLEGAL<{}>", name))};
}

[[nodiscard]] std::string sv::Node::to_string(const LHS &lhs,        // NOLINT(*-no-recursion)
                                              const RenameFn &f) {   // NOLINT(*-no-recursion)
  return std::visit([&](auto &&x) { return to_string(x, f); }, lhs); // NOLINT(*-no-recursion)
}
[[nodiscard]] std::string sv::Node::to_string(const Any &kind, const RenameFn &f) {
  return std::visit([&](auto &&x) { return to_string(x, f); }, kind);
}

[[nodiscard]] std::string sv::Node::to_string(const R<Cond> &x, const RenameFn &f) {
  return fmt::format("COND {}", f(x->lhs.name));
}
[[nodiscard]] std::string sv::Node::to_string(const R<Verbatim> &x, const RenameFn &) {
  return x->value;
}
[[nodiscard]] std::string sv::Node::to_string(const R<Ref> &x, const RenameFn &f) {
  return fmt::format("%{}", f(x->base.name));
}
[[nodiscard]] std::string sv::Node::to_string(const R<MemRef> &x,         // NOLINT(*-no-recursion)
                                              const RenameFn &f) {        // NOLINT(*-no-recursion)
  auto show = [&](auto &maybe, auto prefix) {                             // NOLINT(*-no-recursion)
    return maybe ^ fold([&](auto r) { return prefix + to_string(r, f); }, // NOLINT(*-no-recursion)
                        []() { return ""s; });                            // NOLINT(*-no-recursion)
  };
  return fmt::format("{}[{}{}{}]", //
                     to_string(x->base, f), show(x->offset, "+"), show(x->idx, "+"),
                     show(x->step, "*"), show(x->idx2, "+"));
}

[[nodiscard]] std::string sv::Node::to_string(const R<Select> &x,  // NOLINT(*-no-recursion)
                                              const RenameFn &f) { // NOLINT(*-no-recursion)
  return x->chain |
         mk_string("->", [&](auto &p) { return to_string(p, f); }); // NOLINT(*-no-recursion)
}
[[nodiscard]] std::string sv::Node::to_string(const R<Call> &x, const RenameFn &f) {
  return fmt::format("{} = CALL", to_string(x->lhs, f));
}
[[nodiscard]] std::string sv::Node::to_string(const R<Set> &x, const RenameFn &f) {
  return fmt::format("{} = OP<{}>", to_string(x->lhs, f), x->op);
}

[[nodiscard]] std::string sv::Node::to_string(const RenameFn &f, bool comments) const {
  return fmt::format("{} {}", to_string(kind, f),
                     (!comments || comment.empty() ? "" : "# " + comment));
}

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

sv::GimpleUprootPass::GimpleUprootPass(gcc::context *ctx, std::string basename,
                                       std::string afterPass, std::filesystem::path namedTreePath,
                                       std::filesystem::path unnamedTreePath)
    : gimple_opt_pass(data, ctx), sv::NTreeVisitor<Node, void>(&root),
      basename(std::move(basename)), afterPass(std::move(afterPass)),
      namedTreePath(std::move(namedTreePath)), unnamedTreePath(std::move(unnamedTreePath)) {
  root.value = Node{r<Ref>(Name{this->basename}), {}, sv::Location{}};
}

static std::string to_string(tree tree) { return print_generic_expr_to_str(tree); }

static std::optional<sv::LHS> reprLHS(tree tree);
static sv::LHS reprLHSStrict(tree tree) { // NOLINT(*-no-recursion)
  if (auto lhs = reprLHS(tree); lhs) {
    return *lhs;
  } else {
    return r<sv::Ref>(sv::Ref::illegal(
        fmt::format("ILLEGAL[{}]<{}>", get_tree_code_name(TREE_CODE(tree)), to_string(tree))));
  }
}

static std::optional<sv::LHS> reprLHS(tree tree) { // NOLINT(*-no-recursion)
  if (!tree) { return r<sv::Verbatim>("<NULL>"); }
  if (is_gimple_constant(tree)) return r<sv::Verbatim>(fmt::format("^{}", to_string(tree)));
  switch (auto code = TREE_CODE(tree); code) {
    case ADDR_EXPR: return r<sv::Ref>(sv::Name{fmt::format("@{}", to_string(tree))});
    case RESULT_DECL: [[fallthrough]]; // TODO not entirely sure about this one, exists only once?
    case SSA_NAME: [[fallthrough]];
    case PARM_DECL: [[fallthrough]];
    case VAR_DECL: [[fallthrough]];
    case FIELD_DECL:
      return r<sv::Ref>(sv::Name{fmt::format("{}({})", to_string(tree), get_tree_code_name(code))});
    case TARGET_MEM_REF: {
      auto base = TREE_OPERAND(tree, 0), offset = TREE_OPERAND(tree, 1),
           idx = TREE_OPERAND(tree, 1), step = TREE_OPERAND(tree, 1), idx2 = TREE_OPERAND(tree, 1);
      auto arg = [&](::tree t) { // NOLINT(*-no-recursion)
        return t ? std::optional{reprLHSStrict(t)} : std::nullopt;
      };
      return r<sv::MemRef>(reprLHSStrict(base), arg(offset), arg(idx), arg(step), arg(idx2));
    }
    case ARRAY_REF: {
      auto base = TREE_OPERAND(tree, 0), index = TREE_OPERAND(tree, 1);
      return r<sv::MemRef>(reprLHSStrict(base), std::nullopt,
                           index ? std::optional{reprLHSStrict(index)} : std::nullopt);
    }
    case MEM_REF: {
      auto base = TREE_OPERAND(tree, 0), offset = TREE_OPERAND(tree, 1);
      return r<sv::MemRef>(reprLHSStrict(base),
                           offset ? std::optional{reprLHSStrict(offset)} : std::nullopt);
    }
    case COMPONENT_REF: { // this is a NULL_TREE terminated list of selects
      auto select = r<sv::Select>();
      for (int i = 0; i < TREE_OPERAND_LENGTH(tree); ++i) {
        if (auto path = TREE_OPERAND(tree, i); path) {
          select->chain.emplace_back(reprLHS(path).value_or(r<sv::Ref>(sv::Ref::illegal(fmt::format(
              "Chain: {}({})", to_string(tree), get_tree_code_name(TREE_CODE(tree)))))));
        }
      }
      return select;
    }
    default: return {};
  }
}

sv::Name sv::GimpleUprootPass::mkName() { return {fmt::format("_uproot_syth_{}_", nameCounter++)}; }

void sv::GimpleUprootPass::repr(internal_fn fn) {
  single(Node{r<sv::Verbatim>(fmt::format("#{}", internal_fn_name(fn))), {}, resolveLocation(fn)});
}

void sv::GimpleUprootPass::repr(tree tree, const std::string &comment) { // NOLINT(*-no-recursion)
  if (!tree) {
    single(Node{r<sv::Verbatim>("<NULL>"), comment, sv::Location{}});
    return;
  }
  auto loc = resolveLocation(tree);
  if (auto lhs = reprLHS(tree); lhs) {
    single(std::visit([&](auto &&x) { return Node{x, comment, loc}; }, *lhs));
    return;
  }

  switch (auto code = TREE_CODE(tree); code) {
    case ARRAY_REF:
      single(Node{r<Set>(reprLHSStrict(TREE_OPERAND(tree, 0)), to_string((TREE_OPERAND(tree, 1)))),
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
  }
}

void sv::GimpleUprootPass::repr(gimple *expr, // NOLINT(*-no-recursion)
                                const std::string &comment) {
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

                repr(
                    gimple_phi_arg_def(expr, i),
                    std::string(fmt::format("from L{}", gimple_phi_arg_edge(expr, i)->src->index)));
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

unsigned int sv::GimpleUprootPass::execute(function *f) {
  SV_INFOF("[uproot] Witnessed: {}", function_name(f));
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

void sv::GimpleUprootPass::flush() {
  size_t nodes{};
  root.postOrderWalk([&](auto, auto) { nodes++; });
  auto dump = [&](auto path, auto f) {
    auto tree = root.map<sv::SNode>(f);
    std::ofstream out(path, std::ios::out);
    if (!out) SV_WARNF("[uproot] Unable to open {} for writing", path);
    else {
      out << nlohmann::json(tree);
      SV_INFOF("[uproot] Wrote {} nodes to {}", nodes, path);
    }
  };

  dump(unnamedTreePath, [](auto &r) {
    return sv::SNode{r.to_string([](auto x) { return ""; }, false), r.location};
  });
  dump(namedTreePath,
       [](auto &r) { return sv::SNode{r.to_string([](auto x) { return x; }, true), r.location}; });
}
