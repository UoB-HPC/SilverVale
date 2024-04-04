#pragma once

#include <iosfwd>
#include <ostream>
#include <string>
#include <type_traits>
#include <vector>

#include "clang/AST/RecursiveASTVisitor.h"
#include "json.hpp"

namespace p3md {

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

template <typename T> struct SemanticNode;
template <typename T> class SemanticTreeIterator {
  std::stack<SemanticNode<T> *> stack;

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = SemanticNode<T>;
  using difference_type = std::ptrdiff_t;
  using pointer = value_type *;
  using reference = value_type &;

  SemanticTreeIterator() = default;
  explicit SemanticTreeIterator(SemanticNode<T> *node) {
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

template <typename T> struct SemanticNode {
  T value;
  std::vector<SemanticNode<T>> children;

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(SemanticNode<T>, value, children);

  template <typename Show> void print(Show show, std::ostream &out) const {
    printTree<SemanticNode>(
        0, {}, *this, out, [&](const SemanticNode<T> &n) { return show(n.value); },
        [&](const SemanticNode<T> &n) { return n.children; });
  }

  [[nodiscard]] bool operator!=(const SemanticNode<T> &rhs) const { return !rhs.operator==(*this); }
  [[nodiscard]] bool operator==(const SemanticNode<T> &rhs) const {
    return value == rhs.value && children == rhs.children;
  }
  template <typename U>
  friend std::ostream &operator<<(std::ostream &os, const SemanticNode<U> &node);

  template <typename F> void walk(F f) const {
    static_assert(std::is_same_v<std::invoke_result_t<F, const SemanticNode<T> &>, bool>);
    if (f(*this)) {
      for (auto &child : children)
        walk<F>(child);
    }
  }

  [[nodiscard]] SemanticTreeIterator<T> begin() { return SemanticTreeIterator<T>{this}; }
  [[nodiscard]] SemanticTreeIterator<T> end() const { return SemanticTreeIterator<T>{}; }
};

template <typename U>
std::ostream &operator<<(std::ostream &os, const SemanticNode<U> &node) { // NOLINT(*-no-recursion)
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

namespace {
template <typename Ret, typename Arg, typename... Rest> Arg arg0_helper(Ret (*)(Arg, Rest...));
template <typename Ret, typename F, typename Arg, typename... Rest>
Arg arg0_helper(Ret (F::*)(Arg, Rest...));
template <typename Ret, typename F, typename Arg, typename... Rest>
Arg arg0_helper(Ret (F::*)(Arg, Rest...) const);
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
} // namespace

class TreeSemanticVisitor : public clang::RecursiveASTVisitor<TreeSemanticVisitor> {

public:
  struct Option {
    bool normaliseVarName;
    bool normaliseFnName;
    std::vector<std::string> roots;
  };

private:
  SemanticNode<std::string> *node;
  clang::ASTContext &Context;
  Option option;

  template <typename F, typename... Args> [[nodiscard]] bool scoped(F f, Args... args) {
    SemanticNode<std::string> *prev = node;
    node = &node->children.emplace_back(std::forward<Args &&>(args)...);
    auto x = f();
    node = prev;
    return x;
  }

  template <typename... Args> bool single(Args... args) {
    node->children.emplace_back(std::forward<Args &&>(args)...);
    return true;
  }

public:
  TreeSemanticVisitor(SemanticNode<std::string> *root, clang::ASTContext &Context, Option option);

  bool TraverseDecl(clang::Decl *decl);
  bool TraverseStmt(clang::Stmt *stmt);
};

} // namespace p3md
