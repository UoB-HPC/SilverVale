#include <fstream>
#include <iostream>
#include <thread>

#include "catch2/catch_test_macros.hpp"
#include "catch2/generators/catch_generators.hpp"

#include "agv/glob.h"
#include "agv/tool_index.h"
#include "agv/tool_inspect.h"
#include "agv/tool_script.h"
#include "fixture.h"
#include "fmt/core.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"

using namespace aspartame;

using namespace clang;
using namespace clang::tooling;
using namespace llvm;

static bool fileExist(const std::string &file) {
  std::ifstream infile(file);
  return infile.good();
}

TEST_CASE("database") {

  auto baseGlobs = std::vector<std::string>{"*/db.json", "*/*main.cpp.pch.zstd"};

  auto [dir, model, globs] =
      GENERATE(std::tuple{FIXTURE_MICROSTREAM_SERIAL_DIR, "serial",              //
                          std::vector{"*/*main.bc"}},                            //
               std::tuple{FIXTURE_MICROSTREAM_OMP_DIR, "omp",                    //
                          std::vector{"*/*main.bc"}},                            //
               std::tuple{FIXTURE_MICROSTREAM_OMP_TARGET_DIR, "omp_target",      //
                          std::vector{"*/*main.bc", "*/*main-openmp-*.bc"}},     //
               std::tuple{FIXTURE_MICROSTREAM_HIP_DIR, "hip",                    //
                          std::vector{"*/*main-host-*.bc", "*/*main-hip-*.bc"}}, //
               std::tuple{FIXTURE_MICROSTREAM_CUDA_DIR, "cuda",                  //
                          std::vector{"*/*main.bc", "*/*main-cuda-*.bc"}});      //

  auto out = fmt::format("{}/microstream_{}_db", FIXTURE_TMP_DIR, model);

  DYNAMIC_SECTION("index " << model) {
    int code = agv::index::run(agv::index::Options{
        .buildDir = dir,
        .sourceGlobs = {"*"},
        .argsBefore = {},
        .argsAfter = {},
        .clangResourceDir = FIXTURE_CLANG_RESOURCE_DIR,
        .outDir = out,
        .clearOutDir = true,
        .verbose = true,
        .noCompress = false,
        .maxThreads = static_cast<int>(std::thread::hardware_concurrency()),

    });
    CHECK(code == 0);
    //    CHECK(fileExist(out + "/db.json"));
    //    CHECK(fileExist(out + "/0.main.cpp.pch.zstd"));
    //    CHECK(fileExist(out + "/0.main.bc"));

    std::error_code walkError{};
    for (sys::fs::directory_iterator it = sys::fs::directory_iterator(out, walkError), itEnd;
         it != itEnd && !walkError; it.increment(walkError)) {
      std::string bcFile = sys::path::filename(it->path()).str();
      CHECK((baseGlobs ^ concat(globs) ^ exists([&](auto glob) {
               INFO("Validating " << glob << " against " << it->path());
               return std::regex_match(it->path(), agv::globToRegex(glob));
             })));
    }

    if (walkError) FAIL("cannot walk directory" << out);
  }

  DYNAMIC_SECTION("inspect " << model) {
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

  DYNAMIC_SECTION("script " << model) {
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