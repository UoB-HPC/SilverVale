#pragma once
#include <iosfwd>
#include <stack>
#include <string>
#include <type_traits>
#include <vector>

#include "nlohmann/json.hpp"

namespace agv {

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

  template <typename Show> void print(std::ostream &out, Show show) const {
    printTree<SemanticTree>(
        *this, out, [&](const SemanticTree<T> &n) { return show(n.value); },
        [&](const SemanticTree<T> &n) { return n.children; });
  }

  void print(std::ostream &out) const {
    print(out, [](auto x) { return x; });
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
  U traverse(Alloc alloc, Insert insert) const {
    U n = alloc(value);
    for (const auto &child : children) {
      insert(n, std::move(child.template traverse<U, Alloc, Insert>(alloc, insert)));
    }
    return n;
  }

  template <typename U, typename F> SemanticTree<U> map(F f) const {
    SemanticTree<U> n(f(value), std::vector<SemanticTree<U>>(children.size()));
    for (size_t i = 0; i < children.size(); ++i) {
      n.children[i] = children[i].template map<U>(f);
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

} // namespace agv
