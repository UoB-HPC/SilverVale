#include "catch2/catch_test_macros.hpp"
#include <iostream>

#include "sv/model.h"
#include "sv/tree.h"

#include "aspartame/view.hpp"

using namespace aspartame;
using namespace sv;

using STree = NTree<std::string>;
const STree fixture = STree{"foo",
                            {STree{"bar", {}}, STree{"baz",
                                                     {
                                                         STree{"a",
                                                               {
                                                                   STree{"b",
                                                                         {
                                                                             STree{"c", {}},
                                                                         }},

                                                               }},

                                                     }}}};

TEST_CASE("tree") {
  const Tree t(fixture.map<SNode>([](auto x) { return SNode{x, {}}; }));
  SECTION("width") { CHECK(t.maxWidth() == 2); }
  SECTION("depth") { CHECK(t.maxDepth() == 4); }
  SECTION("nodes") { CHECK(t.nodes() == 6); }
}

TEST_CASE("stree-prune1") {
  auto actual = fixture;
  auto keep = std::unordered_set<std::string>{"c", "a"};
  actual.pruneInplace([&](auto x) { return keep.contains(x); });
  CHECK(actual == STree{"foo",
                        {STree{"baz",
                               {
                                   STree{"a", {STree{"b", {STree{"c", {}}}}}},

                               }}}});
}

TEST_CASE("stree-prune2") {
  auto actual = fixture;
  auto keep = std::unordered_set<std::string>{"foo"};
  actual.pruneInplace([&](auto x) { return keep.contains(x); });
  CHECK(actual == STree{"foo", {}});
}

TEST_CASE("stree-prune3") {
  auto actual = fixture;
  auto keep = std::unordered_set<std::string>{"bar"};
  actual.pruneInplace([&](auto x) { return keep.contains(x); });
  CHECK(actual == STree{"foo", {STree{"bar", {}}}});
}

TEST_CASE("stree-walk1") {
  using M = std::pair<bool, std::string>;
  auto keep = std::unordered_set<std::string>{"a"};
  auto actual = fixture.template map<M>([&](auto &x) -> M { return {keep.contains(x), x}; });
  actual.postOrderWalkInplace([](auto &x, auto d) {
    if (!x.value.first) {
      x.value.first = x.children | exists([](auto &v) { return v.value.first; });
    }
  });
  auto xs = actual                                         //
            | filter([](auto x) { return x.value.first; }) //
            | map([](auto x) { return x.value.second; })   //
            | to_vector();

  CHECK(xs == std::vector<std::string>{"foo", "baz", "a"});
}
