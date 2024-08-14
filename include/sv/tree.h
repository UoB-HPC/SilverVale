#pragma once
#include <iosfwd>
#include <ostream>
#include <sstream>
#include <stack>
#include <string>
#include <type_traits>
#include <vector>

#include "nlohmann/json.hpp"

namespace sv {

namespace {
template <typename Ret, typename Arg, typename... Rest> Arg arg0_helper(Ret (*)(Arg, Rest...));
template <typename Ret, typename F, typename Arg, typename... Rest>
Arg arg0_helper(Ret (F::*)(Arg, Rest...));
template <typename Ret, typename F, typename Arg, typename... Rest>
Arg arg0_helper(Ret (F::*)(Arg, Rest...) const);
template <typename F> decltype(arg0_helper(&F::operator())) arg0_helper(F);

} // namespace

template <typename T> using arg0_t = decltype(arg0_helper(std::declval<T>()));

template <typename N>
void printTree(const N &node,                                                  //
               std::ostream &out,                                              //
               const std::function<std::string(const N &)> &name,              //
               const std::function<const std::vector<N>(const N &)> &children, //
               int depth = 0, std::vector<bool> branch = {}) {
  for (int i = 0; i < depth - 1; ++i)
    out << (branch[i] ? "│  " : "   ");
  if (depth > 0) out << (branch[depth - 1] ? "├─ " : "╰─ ");
  out << name(node) << "\n";
  branch.push_back(true);
  const auto xs = children(node);
  for (size_t i = 0; i < xs.size(); ++i) {
    branch[depth] = (i + 1) < xs.size();
    printTree<N>(xs[i], out, name, children, depth + 1, branch);
  }
}

template <typename T> struct NTree;
template <typename T> class NTreeIterator {
  std::stack<const NTree<T> *> stack{};

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = const NTree<T>;
  using difference_type = std::ptrdiff_t;
  using pointer = value_type *;
  using reference = value_type &;

  NTreeIterator() = default;
  explicit NTreeIterator(const NTree<T> *node) {
    if (node) stack.push(node);
  }

  NTreeIterator &operator++() {
    if (!stack.empty()) {
      auto *current = stack.top();
      stack.pop();
      for (auto it = current->children.rbegin(); it != current->children.rend(); ++it)
        stack.push(&(*it));
    }
    return *this;
  }

  NTreeIterator operator++(int) { // NOLINT(*-dcl21-cpp)
    NTreeIterator temp = *this;
    ++(*this);
    return temp;
  }

  reference operator*() const { return *stack.top(); }
  pointer operator->() const { return stack.top(); }

  bool operator==(const NTreeIterator &other) const { return stack == other.stack; }
  bool operator!=(const NTreeIterator &other) const { return !other.operator==(*this); }
};

struct Location {
  std::string path;
  size_t line, col;
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(Location, path, line, col);
  bool operator==(const Location &rhs) const {
    return path == rhs.path && line == rhs.line && col == rhs.col;
  }
  bool operator!=(const Location &rhs) const { return !(rhs == *this); }
  friend std::ostream &operator<<(std::ostream &os, const Location &loc) {
    return os << loc.path << ":" << loc.line << ":" << loc.col;
  }
};

struct SNode {
  std::string data;
  Location location;
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(SNode, data, location);
  bool operator==(const SNode &rhs) const { return data == rhs.data && location == rhs.location; }
  bool operator!=(const SNode &rhs) const { return !(rhs == *this); }
  friend std::ostream &operator<<(std::ostream &os, const SNode &node) {
    return os << node.data << " (" << node.location << ")";
  }
};

template <typename T> struct NTree {

private:
  template <typename F, typename TT> static void walkImpl(TT &tree, F &&f, size_t depth = 0) {
    static_assert(std::is_same_v<std::invoke_result_t<F, TT &, size_t>, bool>);
    if (f(tree, depth)) {
      for (auto &child : tree.children) {
        walkImpl(child, std::forward<F>(f), depth + 1);
      }
    }
  }

public:
  T value;
  std::vector<NTree<T>> children;

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(NTree<T>, value, children);

  template <typename Show> void print(std::ostream &out, Show show) const {
    printTree<NTree>(
        *this, out, [&](const NTree<T> &n) { return show(n.value); },
        [&](const NTree<T> &n) { return n.children; });
  }

  void print(std::ostream &out) const {
    print(out, [](auto x) {
      std::stringstream out;
      out << x;
      return out.str();
    });
  }

  [[nodiscard]] bool operator!=(const NTree<T> &rhs) const { return !rhs.operator==(*this); }
  [[nodiscard]] bool operator==(const NTree<T> &rhs) const {
    return value == rhs.value && children == rhs.children;
  }
  template <typename U> friend std::ostream &operator<<(std::ostream &os, const NTree<U> &node);

  template <typename F> NTree<T> &postOrderWalkInplace(F f, size_t depth = 0) {
    static_assert(std::is_same_v<std::invoke_result_t<F, NTree<T> &, size_t>, void>);
    for (auto &child : children)
      child.template postOrderWalkInplace<F>(f, depth + 1);
    f(*this, depth);
    return *this;
  }

  template <typename F> NTree<T> &preOrderWalkInplace(F f, size_t depth = 0) {
    static_assert(std::is_same_v<std::invoke_result_t<F, NTree<T> &, size_t>, void>);
    f(*this, depth);
    for (auto &child : children)
      child.template preOrderWalkInplace<F>(f, depth + 1);
    return *this;
  }

  template <typename F> const NTree<T> &preOrderWalk(F f, size_t depth = 0) const {
    static_assert(std::is_same_v<std::invoke_result_t<F, const NTree<T> &, size_t>, bool>);
    if (f(*this, depth)) {
      for (auto &child : children)
        child.template preOrderWalk<F>(f, depth + 1);
    }
    return *this;
  }

  template <typename F> const NTree<T> &postOrderWalk(F f, size_t depth = 0) const {
    static_assert(std::is_same_v<std::invoke_result_t<F, const NTree<T> &, size_t>, void>);
    for (auto &child : children)
      child.template postOrderWalk<F>(f, depth + 1);
    f(*this, depth);
    return *this;
  }

  template <typename U, typename Alloc, typename Insert>
  [[nodiscard]] U traverse(Alloc alloc, Insert insert) const {
    U n = alloc(value);
    for (const auto &child : children) {
      insert(n, std::move(child.template traverse<U, Alloc, Insert>(alloc, insert)));
    }
    return n;
  }

  template <typename U, typename F> [[nodiscard]] NTree<U> map(F f) const {
    NTree<U> n(f(value), std::vector<NTree<U>>(children.size()));
    for (size_t i = 0; i < children.size(); ++i) {
      n.children[i] = children[i].template map<U>(f);
    }
    return n;
  }

  template <typename P> bool pruneInplace(P predicate) {
    auto it = children.begin();
    while (it != children.end()) {
      if (!it->template pruneInplace<P>(predicate)) it = children.erase(it);
      else ++it;
    }
    return predicate(value) || !children.empty();
  }

  [[nodiscard]] NTreeIterator<T> begin() const { return NTreeIterator<T>{this}; }
  [[nodiscard]] NTreeIterator<T> end() const { return NTreeIterator<T>{}; }
};

template <typename U>
std::ostream &operator<<(std::ostream &os, const NTree<U> &node) { // NOLINT(*-no-recursion)
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

template <typename T, typename R> class NTreeVisitor {
protected:
  NTree<T> *node{};
  template <typename F, typename... Args> [[nodiscard]] R scoped(F f, Args... args) {
    NTree<T> *prev = node;
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

  explicit NTreeVisitor(NTree<T> *root) : node(root) {}
};

} // namespace sv
