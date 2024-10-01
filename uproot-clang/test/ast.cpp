#include "catch2/catch_test_macros.hpp"
#include <iostream>

#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"

#include "aspartame/optional.hpp"
#include "aspartame/vector.hpp"

#include "../tree_clangast.h"
#include "../tree_llvmir.h"

using namespace clang;
using namespace clang::tooling;
using namespace aspartame;
using namespace sv;

// main(...)
//  exec clang -cc1 ""
//  runner

std::unique_ptr<ASTUnit> makeASTUnit(const std::string &content) {

  std::string error;
  // FIXME use CMake detected value
  auto db = FixedCompilationDatabase::loadFromBuffer(".", "-I/usr/lib/clang/18/include -fopenmp" , error);
  CHECK(error == ""); // NOLINT(*-container-size-empty)

  ClangTool Tool(*db, {"file.cpp"});
  Tool.mapVirtualFile("file.cpp", content);
  std::vector<std::unique_ptr<ASTUnit>> xs;
  CHECK(Tool.buildASTs(xs) == 0);
  CHECK(xs.size() == 1);
  return std::move(xs[0]);
}

std::vector<NTree<SNode>> makeNodes(const sv::ClangASTSemanticTreeVisitor::Option &option,
                                    const std::string &content) {
  auto unit = makeASTUnit(content);
  return topLevelDeclsInMainFile(*unit) ^ map([&](auto decl) {
           NTree<SNode> root{SNode{"root", Location{}}, {}};
           ClangASTSemanticTreeVisitor V(&root, unit->getASTContext(), option);
           V.TraverseDecl(decl);
           //           decl->dump();
           return root;
         });
}


TEST_CASE("paper") {


  auto trees =
      makeNodes(
          {.inlineCalls = false, .normaliseVarName = false, .normaliseFnName = false, .roots = {""}},
          R"(
int foo(){
  return 1+2;
}

int bar(){
  int b = 0;
  return 3;
}
)");

  trees[0].print(std::cout ,  [](auto x){ return x.data;});
  trees[1].print(std::cout ,  [](auto x){ return x.data;});

//  std::cout << sv::Diff::apted(sv::Tree(trees[0]), sv::Tree(trees[1])) << std::endl;

}


TEST_CASE("cpp-inline") {
  auto trees =
      makeNodes(
          {.inlineCalls = true, .normaliseVarName = false, .normaliseFnName = false, .roots = {""}},
          R"(
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
        return x.template map<std::string>([](auto n) { return n.data; }).children ^ head_maybe() ^
               filter([](auto v) { return v.value == "Function: a" || v.value == "Function: b"; }) ^
               map([](auto n) { return n.children; });
      });

  REQUIRE(trees.size() == 2);
  CHECK(trees[0] == trees[1]);
}

TEST_CASE("cpp-inlines-invariant") {
  auto trees =
      makeNodes(
          {.inlineCalls = true, .normaliseVarName = true, .normaliseFnName = false, .roots = {""}},
          R"(
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
        return x.template map<std::string>([](auto n) { return n.data; }).children ^ head_maybe() ^
               filter([](auto v) { return v.value == "Function: a" || v.value == "Function: b"; }) ^
               map([](auto n) { return n.children; });
      });

  REQUIRE(trees.size() == 2);
  CHECK(trees[0] == trees[1]);
}

TEST_CASE("cpp-two-layers") {
  auto trees =
      makeNodes(
          {.inlineCalls = true, .normaliseVarName = true, .normaliseFnName = false, .roots = {""}},
          R"(
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
        return x.template map<std::string>([](auto n) { return n.data; }).children ^ head_maybe() ^
               filter([](auto v) { return v.value == "Function: a" || v.value == "Function: b"; }) ^
               map([](auto n) { return n.children; });
      });
  REQUIRE(trees.size() == 2);
  CHECK(trees[0] == trees[1]);
}
