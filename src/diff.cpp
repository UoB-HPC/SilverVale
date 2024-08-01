#include "sv/diff.h"
#include "dtl/dtl.hpp"

namespace topdiff {
#include "apted_tree_index.h"
#include "node.h"
#include "unit_cost_model.h"
} // namespace topdiff

struct StringLabel {
  std::string label{};
  explicit StringLabel(std::string label) : label(std::move(label)) {}
  bool operator==(const StringLabel &other) const { return (label == other.to_string()); }
  [[nodiscard]] unsigned int get_type() const { return 0; }
  [[nodiscard]] const std::string &get_label() const { return label; }
  [[nodiscard]] const std::string &to_string() const { return label; }
};

namespace std {
template <> struct hash<StringLabel> {
  typedef StringLabel argument_type;

  size_t operator()(StringLabel const &s) const noexcept {
    size_t const h(std::hash<std::string>{}(s.label));
    return h;
  }
};
} // namespace std

static topdiff::node::Node<StringLabel> makeTree(const sv::NTree<std::string> &tree) {
  return tree.template traverse<topdiff::node::Node<StringLabel>>(
      [](const auto &v) { return topdiff::node::Node<StringLabel>(StringLabel(v)); },
      [](auto &n, const auto &x) { n.add_child(x); });
}

namespace sv {

double Diff::apted(const sv::Tree &lhsTree, const sv::Tree &rhsTree) {
  using CostModelLD = topdiff::cost_model::UnitCostModelLD<StringLabel>;
  using LabelDictionary = topdiff::label::LabelDictionary<StringLabel>;
  LabelDictionary ld;
  CostModelLD ucm(ld);

  topdiff::node::Node<StringLabel> lhs =
      makeTree(lhsTree.root.map<std::string>([](auto s) { return s.data; }));
  topdiff::node::Node<StringLabel> rhs =
      makeTree(rhsTree.root.map<std::string>([](auto s) { return s.data; }));

  topdiff::ted::APTEDTreeIndex<CostModelLD, topdiff::node::TreeIndexAPTED> apted_algorithm(ucm);
  topdiff::node::TreeIndexAPTED ti1;
  topdiff::node::index_tree(ti1, lhs, ld, ucm);
  topdiff::node::TreeIndexAPTED ti2;
  topdiff::node::index_tree(ti2, rhs, ld, ucm);
  return apted_algorithm.ted(ti1, ti2);
}

double Diff::diff(const std::string &lhs, const std::string &rhs) {
  dtl::Diff<char, std::string> d(lhs, rhs);
  d.compose();
  return static_cast<double>(d.getEditDistance());
}

} // namespace sv
