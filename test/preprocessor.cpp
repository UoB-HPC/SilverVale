#include <fstream>
#include <iostream>
#include <unordered_map>

#include "catch2/catch_test_macros.hpp"
#include "fixture.h"
#include "sv/index_common.h"
#include "sv/semantic_ts.h"

#include "aspartame/string.hpp"
#include "aspartame/unordered_map.hpp"
#include "aspartame/vector.hpp"

using namespace aspartame;

TEST_CASE("parse-CPP-linemarkers") {

  std::ifstream read(FIXTURE_PROCESSOR_FILE);

  auto [order, contents] = sv::parseCPPLineMarkers(sv::readFile(FIXTURE_PROCESSOR_FILE));

  auto actual = order ^ map([](auto &p) { return std::filesystem::path(p).filename().string(); });

  REQUIRE(actual.size() >= 5); // should be: processor.cpp, processor.h,
                               // processor_incl_a.h, processor_incl_b.h, processor_incl_c.h

  CHECK(actual[0] == "processor.cpp"); // the first file must be itself
  // then make sure the rest is in this exact order
  std::vector<std::string> rest = {"processor.h",        //
                                   "processor_incl_a.h", //
                                   "processor_incl_b.h", //
                                   "processor_incl_c.h"};
  CHECK(((actual ^ filter([&](auto f) { return rest ^ contains(f); })) == rest));

  for (auto [name, expected] : {
           std::pair{"processor.cpp", "int main() { return 1 + 1 + 1; }\n"
                                      "void end() {}"},
           std::pair{"processor.h", "#pragma foo\n"
                                    "#pragma bar\n"
                                    "#pragma baz\n"
                                    "#pragma omp \"a\"\n"
                                    "void baz() {}"},
           std::pair{"processor_incl_a.h", "void foo() {}"},
           std::pair{"processor_incl_b.h", "void bar() {}"},
           std::pair{"processor_incl_c.h", "void f1();\n"
                                           "void f2();"},
       }) {

    auto entry = contents ^ find([&](auto x, auto) {
                   return std::filesystem::path(x).filename().string() == name;
                 });
    REQUIRE(entry);

    auto normalised = entry->second                                       //
                      ^ lines()                                           //
                      ^ filter([](auto &l) { return !(l ^ is_blank()); }) //
                      ^ mk_string("\n");
    CHECK(normalised == expected);
  }
}
