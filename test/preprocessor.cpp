#include <fstream>
#include <iostream>
#include <unordered_map>

#include "sv/index_common.h"
#include "sv/semantic_ts.h"
#include "catch2/catch_test_macros.hpp"
#include "fixture.h"

#include "aspartame/string.hpp"
#include "aspartame/unordered_map.hpp"
#include "aspartame/vector.hpp"

using namespace aspartame;

TEST_CASE("parse-CPP-linemarkers") {

  std::ifstream read(FIXTURE_PROCESSOR_FILE);

  auto [order, contents] = sv::parseCPPLineMarkers(sv::readFile(FIXTURE_PROCESSOR_FILE));

  CHECK((order ^ map([](auto &p) { return std::filesystem::path(p).filename().string(); })) ==
        std::vector<std::string>{
            "processor.cpp",
            "<built-in>",
            "<command line>",
            "processor.h",
            "processor_incl_a.h",
            "processor_incl_b.h",
            "processor_incl_c.h",
        });

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
