#include "catch2/catch_test_macros.hpp"

#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"

#include "tree_sitter_cpp/api.h"

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

std::vector<p3md::SemanticTree<std::string>>
makeNodes(const p3md::ClangASTSemanticTreeVisitor::Option &option, const std::string &content) {
  auto unit = makeASTUnit(content);
  return p3md::topLevelDeclsInMainFile(*unit) ^ map([&](auto decl) {
           p3md::SemanticTree<std::string> root{"root", {}};
           p3md::ClangASTSemanticTreeVisitor V(&root, unit->getASTContext(), option);
           V.TraverseDecl(decl);
           //           decl->dump();
           return root;
         });
}

TEST_CASE("ts-normalise-comments") {

  auto source = R"(
//*Foo*/
int main(/**/)/* a */{
// a
return 0 /**/ + 1;// b
}//
//
// a // b
)";

  CHECK(p3md::TsTree(source, tree_sitter_cpp()).deleteNodes("comment").source == R"(

int main(){

return 0  + 1;
}


)");
}

TEST_CASE("inline") {
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
        return x.children ^ head_maybe() ^
               filter([](auto v) { return v.value == "Function: a" || v.value == "Function: b"; }) ^
               map([](auto n) { return n.children; });
      });

  REQUIRE(trees.size() == 2);
  CHECK(trees[0] == trees[1]);
}

TEST_CASE("inlines invariant") {
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
        return x.children ^ head_maybe() ^
               filter([](auto v) { return v.value == "Function: a" || v.value == "Function: b"; }) ^
               map([](auto n) { return n.children; });
      });

  REQUIRE(trees.size() == 2);
  CHECK(trees[0] == trees[1]);
}

TEST_CASE("two layers") {
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
        return x.children ^ head_maybe() ^
               filter([](auto v) { return v.value == "Function: a" || v.value == "Function: b"; }) ^
               map([](auto n) { return n.children; });
      });
  REQUIRE(trees.size() == 2);
  CHECK(trees[0] == trees[1]);
}

TEST_CASE("delete comments") {

  std::string src = R"(
/*
 * Foo
 */

#include <stdio.h>
#include <stdlib.h>

/*foo*/// x

int a = 3;// foo
//
void a() /*b*/ {
// c
return 42;// w
}// d

)";

  std::string expected = R"(


#include <stdio.h>
#include <stdlib.h>



int a = 3;

void a()  {

return 42;
}

)";

  CHECK(p3md::TsTree(src, tree_sitter_cpp()).deleteNodes("comment").source == expected);
}

TEST_CASE("normalise ws") {

  std::string src = R"(


#include <stdio.h>
#include <stdlib.h>



int a = 3;

void a ( )        {

return 42;
}

auto f  = [   & ] ( auto   y )  {    return    2    ;   }   ;

)";

  std::string expected = R"(
#include <stdio.h>
#include <stdlib.h>
int a=3;
void a(){
return 42;
}
auto f = [&](auto y){return 2;};
)";

  CHECK(p3md::TsTree(src, tree_sitter_cpp()).normaliseWhitespaces(1).source == expected);
}