#include <fstream>
#include <iostream>
#include <unordered_map>

#include "catch2/catch_test_macros.hpp"
#include "fixture.h"
#include "fmt/core.h"
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
           std::pair{"processor.cpp", std::vector{std::pair{16, "int main() { return 1 + 1 + 1; }"},
                                                  std::pair{24, "void end() {}"}}},
           std::pair{"processor.h",
                     std::vector{std::pair{8, "#pragma foo"}, std::pair{9, "#pragma bar"},
                                 std::pair{10, "#pragma baz"}, std::pair{11, "#pragma omp \"a\""},
                                 std::pair{12, "void baz() {}"}}},
           std::pair{"processor_incl_a.h", std::vector{std::pair{4, "void foo() {}"}}},
           std::pair{"processor_incl_b.h", std::vector{std::pair{4, "void bar() {}"}}},
           std::pair{"processor_incl_c.h",
                     std::vector{std::pair{1, "void f1();"}, std::pair{2, "void f2();"}}},
       }) {

    auto entry = contents ^ find([&](auto x, auto) {
                   return std::filesystem::path(x).filename().string() == name;
                 });
    REQUIRE(entry);

    auto actualLines = (entry->second ^ lines()) | zip_with_index(1) |
                       map([](auto l, auto idx) { return std::pair{idx, l}; }) | to_vector();
    INFO((actualLines ^
          mk_string("\n", [](auto idx, auto l) { return fmt::format("[{}]{}", idx, l); })));
    for (auto [num, line] : expected) {
      INFO(num);
      INFO(line);
      CHECK((actualLines ^ contains(std::pair{num, std::string(line)})));
    }
  }
}
