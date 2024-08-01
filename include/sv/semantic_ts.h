#pragma once

#include <set>

#include "tree.h"
#include "tree_sitter/api.h"

#include "aspartame/view.hpp"

namespace sv {

[[nodiscard]]
std::pair<std::vector<std::string>, std::unordered_map<std::string, std::string>>
parseCPPLineMarkers(const std::string &iiLines);

struct TsTree {
  std::string name;
  std::string source;
  std::shared_ptr<TSParser> parser;
  std::shared_ptr<TSTree> tree;

  TsTree();
  TsTree(const std::string &name, const std::string &source, const TSLanguage *lang);
  [[nodiscard]] TSNode root() const;
  [[nodiscard]] TsTree without(const std::string &type,
                               const std::optional<TSNode> &node = {}) const;

  [[nodiscard]] TsTree normaliseNewLines(const std::optional<TSNode> &node = {}) const;
  [[nodiscard]] TsTree normaliseWhitespaces(uint32_t maxWS = 1,
                                            const std::optional<TSNode> &node = {}) const;

  [[nodiscard]] std::set<uint32_t> slocLines(               //
      const std::function<bool(const TSNode &)> &mask = {}, //
      const std::optional<TSNode> &node = {}) const;
  [[nodiscard]] std::set<std::pair<uint32_t, uint32_t>> llocRanges( //
      const std::function<bool(const TSNode &)> &mask = {},         //
      const std::optional<TSNode> &node = {}) const;

  [[nodiscard]] TsTree deleteRanges(const std::vector<std::pair<uint32_t, uint32_t>> &ranges) const;

  template <typename F>
  void preOrderWalk(F f, const std::optional<TSNode> &node = {}, bool namedOnly = true) const {
    static_assert(std::is_same_v<std::invoke_result_t<F, const TSNode &>, bool>);
    if (!node) preOrderWalk<F>(f, root());
    else if (f(*node)) {
      for (uint32_t i = 0; i < ts_node_child_count(*node); ++i) {
        if (auto child = ts_node_child(*node, i); namedOnly ? ts_node_is_named(child) : true) {
          preOrderWalk(f, child, namedOnly);
        }
      }
    }
  }

  template <typename U, typename Alloc, typename Insert>
  [[nodiscard]] U traverse(Alloc alloc, Insert insert, int depth = 0,
                           const std::optional<TSNode> &node = {}, bool namedOnly = true) const {
    if (!node) return traverse<U, Alloc, Insert>(alloc, insert, depth, root());
    else {
      U n = alloc(*node);
      for (uint32_t i = 0; i < ts_node_child_count(*node); ++i) {
        if (auto child = ts_node_child(*node, i); namedOnly ? ts_node_is_named(child) : true) {
          insert(n,
                 std::move(traverse<U, Alloc, Insert>(alloc, insert, depth + 1, child, namedOnly)));
        }
      }
      return n;
    }
  }

  template <typename Show>
  void print(Show show, std::ostream &out, const std::optional<TSNode> &node = {}) const {
    using namespace aspartame;
    printTree<TSNode>(
        node.value_or(root()), out, //
        [&](const TSNode &n) { return show(n); },
        [&](const TSNode &n) {
          return exclusive<uint32_t>(0, ts_node_child_count(n))              //
                 | map([&](uint32_t i) { return ts_node_child(n, i); }) //
                 | filter([&](auto &n) { return ts_node_is_named(n); }) //
                 | to_vector();
        });
  }
};

} // namespace sv
