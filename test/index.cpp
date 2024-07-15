#include <fstream>
#include <iostream>
#include <thread>

#include "catch2/catch_test_macros.hpp"

#include "agv/tool_index.h"
#include "agv/tool_inspect.h"
#include "agv/tool_script.h"
#include "fixture.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"

using namespace aspartame;

using namespace clang;
using namespace clang::tooling;

static bool fileExist(const std::string &file) {
  std::ifstream infile(file);
  return infile.good();
}

TEST_CASE("database") {

  auto out = std::string(FIXTURE_TMP_DIR) + "/microstream_db";

  SECTION("index") {
    int code = agv::index::run(agv::index::Options{
        .buildDir = FIXTURE_MICROSTREAM_DIR,
        .sourceGlobs = {"*"},
        .argsBefore = {},
        .argsAfter = {},
        .clangResourceDir = CLANG_RESOURCE_DIR,
        .outDir = out,
        .clearOutDir = true,
        .verbose = true,
        .noCompress = false,
        .maxThreads = static_cast<int>(std::thread::hardware_concurrency()),

    });
    CHECK(code == 0);
    CHECK(fileExist(out + "/db.json"));
    CHECK(fileExist(out + "/0.main.cpp.pch.zstd"));
    CHECK(fileExist(out + "/0.main.bc"));
  }

  SECTION("inspect") {
    auto buffer = std::cout.rdbuf();
    std::ostringstream ss;
    std::cout.rdbuf(ss.rdbuf());
    int code =
        agv::inspect::run(agv::inspect::Options{.dbDir = out, .kind = agv::inspect::Kind::Entry});
    std::cout.rdbuf(buffer);
    CHECK(code == 0);
    auto actual = ss.str() ^ lines();
    REQUIRE(actual.size() == 2);
    CHECK(actual[0] == "entry,deps");
    CHECK(actual[1] ^ contains_slice("microstream/main.cpp,"));
  }

  SECTION("script") {
    std::string scriptPath = std::string(FIXTURE_TMP_DIR) + "/script.lua";
    {
      std::ofstream script(scriptPath, std::ios::trunc);
      REQUIRE(script.is_open());
      script << R"(
print(#arg)
print(arg[1])
local db = Database.fromJsonFile(arg[1] .. "/db.json")

local n = 0
for _ in pairs(db:entries()) do n = n + 1 end
print(n)

local cb = db:load(true, arg[1], {}, function(s) return true end)
print(cb:units()[1]:name())
print(Diff.apted(cb:units()[1]:sTree(), cb:units()[1]:sTree()))
)";
    }
    auto buffer = std::cout.rdbuf();
    std::ostringstream ss;
    std::cout.rdbuf(ss.rdbuf());
    int code = agv::script::run(
        agv::script::Options{.roots = {},
                             .defs = false,
                             .noBuffer = false,
                             .maxThreads = static_cast<int>(std::thread::hardware_concurrency()),
                             .args = {scriptPath, out}});
    std::cout.rdbuf(buffer);
    CHECK(code == 0);

    auto actual = ss.str() ^ lines() ^ filter([](auto &l) { return !(l ^ starts_with("#")); });
    REQUIRE(actual.size() == 5);
    CHECK(actual[0] == "1");
    CHECK(actual[1] == out);
    CHECK(actual[2] == "1");
    CHECK(actual[3] ^ starts_with("main.")); // either main.cpp or main.ii
    CHECK(actual[4] == "0.0");
  }
}