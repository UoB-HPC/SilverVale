#include "catch2/catch_test_macros.hpp"

#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"

#include "aspartame/optional.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "p3md/p3md.h"
#include "p3md/tree.h"

using namespace clang;
using namespace clang::tooling;
using namespace aspartame;

std::unique_ptr<ASTUnit> makeASTUnit(const std::string &content) {

  std::string error;
  auto db = FixedCompilationDatabase::loadFromBuffer(".", "-I/usr/lib/clang/17/include", error);
  CHECK(error == ""); // NOLINT(*-container-size-empty)

  ClangTool Tool(*db, {"file.cpp"});
  Tool.mapVirtualFile("file.cpp", content);
  std::vector<std::unique_ptr<ASTUnit>> xs;
  CHECK(Tool.buildASTs(xs) == 0);
  CHECK(xs.size() == 1);
  return std::move(xs[0]);
}

std::vector<p3md::SemanticNode<std::string>>
makeNodes(const p3md::TreeSemanticVisitor::Option &option, const std::string &content) {
  auto unit = makeASTUnit(content);
  return p3md::topLevelDeclsInMainFile(*unit) ^ map([&](auto decl) {
           p3md::SemanticNode<std::string> root{"root", {}};
           p3md::TreeSemanticVisitor V(&root, unit->getASTContext(), option);
           V.TraverseDecl(decl);
           //           decl->dump();
           return root;
         });
}

TEST_CASE("inline") {
  auto trees =
      makeNodes({.normaliseVarName = false, .normaliseFnName = false, .roots = {""}}, R"(
#include <stdio.h>
#include <stdlib.h>

void foo();

template <typename X> void run(int a, X x) {
  foo();
  x(a + 1);
  foo();
}

int a() {
  int a = 42;
  printf("Hey %d\n", 42);
  foo();
  [&](auto x) { printf("%d\n", x + a); }(a + 1);
  foo();
  return EXIT_SUCCESS;
}

int b() {
  int a = 42;
  printf("Hey %d\n", 42);
  run(a, [&](auto x) { printf("%d\n", x + a); });
  return EXIT_SUCCESS;
}
)") ^ collect([](auto x) {
        return x.children ^ head_maybe() ^
               filter([](auto v) { return v.value == "Function: a" || v.value == "Function: b"; }) ^
               map([](auto n) { return n.children; });
      });

  REQUIRE(trees.size() == 2);
  CHECK(trees[0] == trees[1]);
}

TEST_CASE("inlines invariant") {
  auto trees =
      makeNodes({.normaliseVarName = true, .normaliseFnName = false, .roots = {""}}, R"(
#include <stdio.h>
#include <stdlib.h>

void bar();

template <typename F> void run(int m, F f) {
  bar();
  f(m + 1);
  bar();
}

int a() {
  int aaa = 42;
  printf("Hey %d\n", 42);
  bar();
  [&](auto xxx) { printf("%d\n", xxx + aaa); }(aaa + 1);
  bar();
  return EXIT_SUCCESS;
}

int b() {
  int a = 42;
  printf("Hey %d\n", 42);
  run(a, [&](auto x) { printf("%d\n", x + a); });
  return EXIT_SUCCESS;
}
)") ^ collect([](auto x) {
        return x.children ^ head_maybe() ^
               filter([](auto v) { return v.value == "Function: a" || v.value == "Function: b"; }) ^
               map([](auto n) { return n.children; });
      });

  REQUIRE(trees.size() == 2);
  CHECK(trees[0] == trees[1]);
}

TEST_CASE("two layers") {
  auto trees =
      makeNodes({.normaliseVarName = true, .normaliseFnName = false, .roots = {""}}, R"(
#include <stdio.h>
#include <stdlib.h>

void foo();
void bar(int, int);

template <typename X> void run(int a, X x) {
  foo();
  x(a, a);
}

int a() {
  int a = 42;
  foo();
  bar(a, a);
  return EXIT_SUCCESS;
}

int b() {
  int a = 42;
  run(a, [&](auto x, auto y) { bar(x, y); });
  return EXIT_SUCCESS;
}
)") ^ collect([](auto x) {
        return x.children ^ head_maybe() ^
               filter([](auto v) { return v.value == "Function: a" || v.value == "Function: b"; }) ^
               map([](auto n) { return n.children; });
      });
  REQUIRE(trees.size() == 2);
  CHECK(trees[0] == trees[1]);
}