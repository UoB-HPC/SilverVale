#pragma once

#include <iosfwd>
#include <string>
#include <type_traits>
#include <vector>

#include "aspartame/view.hpp"
#include "json.hpp"
#include "tree_sitter/api.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/ASTUnit.h"
#include "llvm/IR/Module.h"

namespace agv {

namespace {
template <typename Ret, typename Arg, typename... Rest> Arg arg0_helper(Ret (*)(Arg, Rest...));
template <typename Ret, typename F, typename Arg, typename... Rest>
Arg arg0_helper(Ret (F:: *)(Arg, Rest...));
template <typename Ret, typename F, typename Arg, typename... Rest>
Arg arg0_helper(Ret (F:: *)(Arg, Rest...) const);
template <typename F> decltype(arg0_helper(&F::operator())) arg0_helper(F);
template <typename T> using arg0_t = decltype(arg0_helper(std::declval<T>()));
template <typename T, typename Node, typename... Fs> std::optional<T> visitDyn(Node n, Fs... fs) {
  std::optional<T> result{};
  [[maybe_unused]] auto _ = {[&]() {
    if (!result) {
      if (auto x = llvm::dyn_cast<std::remove_pointer_t<arg0_t<Fs>>>(n)) { result = T(fs(x)); }
    }
    return 0;
  }()...};
  return result;
}
template <typename Node, typename... Fs> void visitDyn0(Node n, Fs... fs) {
  [[maybe_unused]] auto _ = ([&]() -> bool {
    if (auto x = llvm::dyn_cast<std::remove_pointer_t<arg0_t<Fs>>>(n)) {
      fs(x);
      return true;
    }
    return false;
  }() || ...);
}
} // namespace

std::vector<clang::Decl *> topLevelDeclsInMainFile(clang::ASTUnit &unit);

template <typename N>
void printTree(int depth, std::vector<bool> branch, const N &node, std::ostream &out,
               const std::function<std::string(N)> &name,
               const std::function<const std::vector<N>(N)> &children) {
  for (int i = 0; i < depth - 1; ++i)
    out << (branch[i] ? "│  " : "   ");
  if (depth > 0) out << (branch[depth - 1] ? "├─ " : "╰─ ");
  out << name(node) << "\n";
  branch.push_back(true);
  const auto &xs = children(node);
  for (size_t i = 0; i < xs.size(); ++i) {
    branch[depth] = (i + 1) < xs.size();
    printTree<N>(depth + 1, branch, xs[i], out, name, children);
  }
}

template <typename T> struct SemanticTree;
template <typename T> class SemanticTreeIterator {
  std::stack<SemanticTree<T> *> stack{};

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = SemanticTree<T>;
  using difference_type = std::ptrdiff_t;
  using pointer = value_type *;
  using reference = value_type &;

  SemanticTreeIterator() = default;
  explicit SemanticTreeIterator(SemanticTree<T> *node) {
    if (node) stack.push(node);
  }

  SemanticTreeIterator &operator++() {
    if (!stack.empty()) {
      auto *current = stack.top();
      stack.pop();
      for (auto it = current->children.rbegin(); it != current->children.rend(); ++it)
        stack.push(&(*it));
    }
    return *this;
  }

  SemanticTreeIterator operator++(int) { // NOLINT(*-dcl21-cpp)
    SemanticTreeIterator temp = *this;
    ++(*this);
    return temp;
  }

  reference operator*() const { return *stack.top(); }
  pointer operator->() const { return stack.top(); }

  bool operator==(const SemanticTreeIterator &other) const { return stack == other.stack; }
  bool operator!=(const SemanticTreeIterator &other) const { return !other.operator==(*this); }
};

template <typename T> struct SemanticTree {
  T value;
  std::vector<SemanticTree<T>> children;

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(SemanticTree<T>, value, children);

  template <typename Show> void print(Show show, std::ostream &out) const {
    printTree<SemanticTree>(
        0, {}, *this, out, [&](const SemanticTree<T> &n) { return show(n.value); },
        [&](const SemanticTree<T> &n) { return n.children; });
  }

  [[nodiscard]] bool operator!=(const SemanticTree<T> &rhs) const { return !rhs.operator==(*this); }
  [[nodiscard]] bool operator==(const SemanticTree<T> &rhs) const {
    return value == rhs.value && children == rhs.children;
  }
  template <typename U>
  friend std::ostream &operator<<(std::ostream &os, const SemanticTree<U> &node);

  template <typename F> void walk(F f, size_t depth = 0) const {
    static_assert(std::is_same_v<std::invoke_result_t<F, const SemanticTree<T> &, size_t>, bool>);
    if (f(*this, depth)) {
      for (const auto &child : children)
        child.walk(f, depth + 1);
    }
  }

  template <typename U, typename Alloc, typename Insert>
  U traverse(Alloc alloc, Insert insert, int depth = 0) const {
    U n = alloc(value);
    for (const auto &child : children) {
      insert(n, std::move(child.template traverse<U, Alloc, Insert>(alloc, insert, depth + 1)));
    }
    return n;
  }

  [[nodiscard]] SemanticTreeIterator<T> begin() const { return SemanticTreeIterator<T>{this}; }
  [[nodiscard]] SemanticTreeIterator<T> end() const { return SemanticTreeIterator<T>{}; }
};

template <typename U>
std::ostream &operator<<(std::ostream &os, const SemanticTree<U> &node) { // NOLINT(*-no-recursion)
  if (node.children.empty()) {
    os << "{\"" << node.value << "\"}";
  } else {
    os << "{\"" << node.value << "\" {";
    for (auto &child : node.children)
      os << child;
    os << "}";
  }
  return os;
}

template <typename T, typename R> class SemanticTreeVisitor {
protected:
  SemanticTree<T> *node{};
  template <typename F, typename... Args> [[nodiscard]] R scoped(F f, Args... args) {
    SemanticTree<T> *prev = node;
    node = &node->children.emplace_back(std::forward<Args &&>(args)...);
    if constexpr (std::is_void_v<R>) {
      f();
      node = prev;
    } else {
      auto x = f();
      node = prev;
      return x;
    }
  }
  template <typename... Args> R single(Args... args) {
    node->children.emplace_back(std::forward<Args &&>(args)...);
    if constexpr (!std::is_void_v<R>) { return {}; }
  }

  explicit SemanticTreeVisitor(SemanticTree<T> *root) : node(root) {}
};

class LLVMIRTreeVisitor : private SemanticTreeVisitor<std::string, void> {
  bool normaliseName;
  [[nodiscard]] std::string named(const std::string &kind, const std::string &name) const;
  void walk(const llvm::Value *fn);

public:
  LLVMIRTreeVisitor(SemanticTree<std::string> *root, const llvm::Module &module,
                    bool normaliseName);
};

class ClangASTSemanticTreeVisitor : private SemanticTreeVisitor<std::string, bool>,
                                    public clang::RecursiveASTVisitor<ClangASTSemanticTreeVisitor> {

public:
  struct Option {
    bool inlineCalls;
    bool normaliseVarName;
    bool normaliseFnName;
    std::vector<std::string> roots;
  };

private:
  clang::ASTContext &Context;
  Option option;

public:
  ClangASTSemanticTreeVisitor(SemanticTree<std::string> *root, clang::ASTContext &Context,
                              Option option);
  bool TraverseDecl(clang::Decl *decl);
  bool TraverseStmt(clang::Stmt *stmt);
};

struct TsTree {

public:
  std::string source;
  std::shared_ptr<TSParser> parser;
  std::shared_ptr<TSTree> tree;
  TsTree();
  TsTree(const std::string &source, const TSLanguage *lang);
  [[nodiscard]] TSNode root() const;
  [[nodiscard]] TsTree deleteNodes(const std::string &type,
                                   const std::optional<TSNode> &node = {}) const;
  [[nodiscard]] TsTree normaliseWhitespaces(size_t maxWhitespaces = 1,
                                            const std::optional<TSNode> &node = {}) const;

  [[nodiscard]] size_t sloc(const std::optional<TSNode> &node = {}) const;
  [[nodiscard]] size_t lloc(const std::optional<TSNode> &node = {}) const;

  template <typename F> void walk(F f, const std::optional<TSNode> &node = {}) const {
    static_assert(std::is_same_v<std::invoke_result_t<F, const TSNode &>, bool>);
    if (!node) walk<F>(f, root());
    else if (f(*node)) {
      for (uint32_t i = 0; i < ts_node_child_count(*node); ++i) {
        walk(f, ts_node_child(*node, i));
      }
    }
  }

  template <typename U, typename Alloc, typename Insert>
  U traverse(Alloc alloc, Insert insert, int depth = 0,
             const std::optional<TSNode> &node = {}) const {
    if (!node) return traverse<U, Alloc, Insert>(alloc, insert, depth, root());
    else {
      U n = alloc(std::string(ts_node_type(*node)));
      for (uint32_t i = 0; i < ts_node_child_count(*node); ++i) {
        insert(n, std::move(traverse<U, Alloc, Insert>(alloc, insert, depth + 1,
                                                       ts_node_child(*node, i))));
      }
      return n;
    }
  }

  template <typename Show>
  void print(Show show, std::ostream &out, const std::optional<TSNode> &node = {}) const {
    using namespace aspartame;
    printTree<TSNode>(
        0, {}, node, out, [&](const TSNode &n) { return show(ts_node_type(n)); },
        [&](const TSNode &n) -> std::vector<TSNode> {
          return iota<uint32_t>(0, ts_node_child_count(n))              //
                 | map([&](uint32_t i) { return ts_node_child(n, i); }) //
                 | to_vector();
        });
  }

private:
  static void deleteNodes(const TSNode &node, const std::string &type, size_t &offset,
                          std::string &out);

  static void normaliseWhitespaces(const TSNode &node, size_t &offset, size_t maxWhitespaces,
                                   std::string &out);
};

} // namespace agv
