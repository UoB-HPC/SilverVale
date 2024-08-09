#pragma once

#include <atomic>
#include <filesystem>
#include <functional>
#include <string>
#include <utility>
#include <variant>

#include "sv/tree.h"

#define SAFE_CTYPE_H

#include "gcc-plugin.h"
#include "tree-pass.h"

namespace sv {

using RenameFn = std::function<std::string(const std::string &)>;
template <typename T> using R = std::shared_ptr<T>;
template <typename... T> using Alternatives = std::variant<R<T>...>;

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
  static Ref illegal(const std::string &name);
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

  [[nodiscard]] static std::string to_string(const LHS &lhs, // NOLINT(*-no-recursion)
                                             const RenameFn &f);
  [[nodiscard]] static std::string to_string(const Any &kind, const RenameFn &f);

  [[nodiscard]] static std::string to_string(const R<Cond> &x, const RenameFn &f);
  [[nodiscard]] static std::string to_string(const R<Verbatim> &x, const RenameFn &);
  [[nodiscard]] static std::string to_string(const R<Ref> &x, const RenameFn &f);
  [[nodiscard]] static std::string to_string(const R<MemRef> &x, // NOLINT(*-no-recursion)
                                             const RenameFn &f);
  [[nodiscard]] static std::string to_string(const R<Select> &x, // NOLINT(*-no-recursion)
                                             const RenameFn &f);
  [[nodiscard]] static std::string to_string(const R<Call> &x, const RenameFn &f);
  [[nodiscard]] static std::string to_string(const R<Set> &x, const RenameFn &f);
  [[nodiscard]] std::string to_string(const RenameFn &f, bool comments = true) const;
};

class GimpleUprootPass : public gimple_opt_pass, sv::NTreeVisitor<Node, void> {

  std::atomic_long nameCounter{};
  sv::NTree<Node> root;
  std::string basename, afterPass;
  std::filesystem::path namedTreePath, unnamedTreePath;

public:
  explicit GimpleUprootPass(gcc::context *ctx, std::string basename, std::string afterPass,
                            std::filesystem::path namedTreePath,
                            std::filesystem::path unnamedTreePath);

  Name mkName();
  void repr(internal_fn fn);
  void repr(tree tree, const std::string &comment = "");
  void repr(gimple *expr, const std::string &comment = "");
  unsigned int execute(function *f) override;
  void flush();
};
} // namespace sv