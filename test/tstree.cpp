#include "catch2/catch_test_macros.hpp"
#include <iostream>

#include "aspartame/optional.hpp"
#include "aspartame/set.hpp"
#include "aspartame/vector.hpp"

#include "sv/tree_ts.h"

#ifdef SKIP // XXX catch2 introduces a SKIP macro which collides with one in tree_sitter_cpp
  #undef SKIP
#endif

#include "tree_sitter_cpp/api.h"
#include "tree_sitter_fortran/api.h"

using namespace aspartame;
using namespace sv;

TEST_CASE("cpp-source-loc") {

  auto source = R"(
//*Foo*/
int main(/**/)/* a */{
// a
#pragma omp
return 0 /**/ + 1;// b
}//
//
// a // b
)";

  const auto &tree = TsTree("", source, tree_sitter_cpp()).without("comment");
  CHECK(tree.source == R"(

int main(){

#pragma omp
return 0  + 1;
}


)");
  SECTION("SLOC") { CHECK(tree.slocLines() == std::set<uint32_t>{3, 5, 6, 7}); }
  SECTION("LLOC") {
    CHECK(tree.llocRanges().size() == 2);
    CHECK((tree.llocRanges() ^ map([&](auto s, auto e) { return tree.source.substr(s, e - s); })) ==
          std::set<std::string>{"#pragma omp", "return 0  + 1;"});
  }
}

TEST_CASE("fortran-source-loc") {

  auto source = R"(
!*Foo*
program main
implicit none
! a
integer :: result
result = 0!/**/ + 1
print *, result! b
end program main
!!
! a ! b

)";

  const auto &tree = TsTree("", source, tree_sitter_fortran()).without("comment");
  CHECK(tree.source == R"(

program main
implicit none

integer :: result
result = 0
print *, result
end program main



)");

  SECTION("SLOC") { CHECK(tree.slocLines() == std::set<uint32_t>{3, 4, 6, 7, 8, 9}); }
  SECTION("LLOC") {
    CHECK(tree.llocRanges().size() == 6);
    CHECK((tree.llocRanges()                          //
           ^ to_vector()                              //
           ^ sort_by([](auto &s, auto) { return s; }) //
           ^ map([&](auto s, auto e) { return tree.source.substr(s, e - s); })) ==
          std::vector<std::string>{
              "program main",
              "implicit none",
              "integer :: result",
              "result = 0",
              "print *, result",
              "end program main",
          });
  }
}

TEST_CASE("cpp-delete-comments") {

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

  CHECK(TsTree("", src, tree_sitter_cpp()).without("comment").source == R"(


#include <stdio.h>
#include <stdlib.h>



int a = 3;

void a()  {

return 42;
}

)");
}

TEST_CASE("cpp-normalise") {

  std::string src = R"(


#include <stdio.h>


#include <stdlib.h>



int a  = 3;

int  a_a   (      )        {     //a

  return  42;
}

auto f  = [   & ]  ( auto   y )  {    return    2    ;   }   ; //

)";

  SECTION("ws") {

    CHECK(TsTree("", src, tree_sitter_cpp()).normaliseWhitespaces().source == R"(


#include <stdio.h>

#include <stdlib.h>

int a = 3;
int a_a ( ) { //a
return 42;
}
auto f = [ & ] ( auto y ) { return 2 ; } ; //

)");
  }

  SECTION("nl") {
    CHECK(TsTree("", src, tree_sitter_cpp()).normaliseNewLines().source == R"(#include <stdio.h>
#include <stdlib.h>
int a  = 3;
int  a_a   (      )        {     //a
  return  42;
}
auto f  = [   & ]  ( auto   y )  {    return    2    ;   }   ; //)");
  }

  SECTION("ws+nl") {
    CHECK(TsTree("", src, tree_sitter_cpp()).normaliseNewLines().normaliseWhitespaces().source ==
          R"(#include <stdio.h>
#include <stdlib.h>
int a = 3;
int a_a ( ) { //a
return 42;
}
auto f = [ & ] ( auto y ) { return 2 ; } ; //)");
  }
}

TEST_CASE("cpp-stmt-multiline") {
  std::string src = R"(int main() { return a ?
0
 :
  1
   ;
)";
  CHECK(TsTree("", src, tree_sitter_cpp()).normaliseNewLines().normaliseWhitespaces().source ==
        R"(int main() { return a ?
0
:
1
;)");
}
