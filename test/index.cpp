#include <fstream>
#include <iostream>
#include <thread>
#include <tuple>

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

TEST_CASE("clang-database") {

  auto gccBaseGlobs = std::vector{"*/*main.sv.json"};
  auto clangBaseGlobs = std::vector{"*/*main.sv.json", "*/*main.pch.zstd"};

  auto [expr, globs, ext] = GENERATE_REF(
      std::tuple{FIXTURE_MICROSTREAM_GCC_SERIAL_EXPR,
                 std::vector{"*/*.main.named.irtree.json", "*/*.main.unnamed.irtree.json"} ^
                     concat(gccBaseGlobs),
                 ".cpp"},

      std::tuple{FIXTURE_MICROSTREAM_GCC_OMP_EXPR,
                 std::vector{"*/*.main.named.irtree.json", "*/*.main.unnamed.irtree.json"} ^
                     concat(gccBaseGlobs),
                 ".cpp"},

      std::tuple{FIXTURE_MICROSTREAM_GCC_SERIAL_F90_EXPR,
                 std::vector{"*/*.main.named.irtree.json", "*/*.main.unnamed.irtree.json"} ^
                     concat(gccBaseGlobs),
                 ".F90"},

      std::tuple{FIXTURE_MICROSTREAM_GCC_OMP_F90_EXPR,
                 std::vector{"*/*.main.named.irtree.json", "*/*.main.unnamed.irtree.json"} ^
                     concat(gccBaseGlobs),
                 ".F90"},

      std::tuple{FIXTURE_MICROSTREAM_CLANG_SERIAL_EXPR,
                 std::vector{"*/*main.bc"} ^ concat(clangBaseGlobs), ".cpp"} //

#ifdef FIXTURE_MICROSTREAM_CLANG_OMP_EXPR
      ,
      std::tuple{FIXTURE_MICROSTREAM_CLANG_OMP_EXPR,
                 std::vector{"*/*main.bc"} ^ concat(clangBaseGlobs), ".cpp"} //
#endif
#ifdef FIXTURE_MICROSTREAM_CLANG_OMP_TARGET_EXPR
      ,
      std::tuple{FIXTURE_MICROSTREAM_CLANG_OMP_TARGET_EXPR,
                 std::vector{"*/*main.bc", "*/*main-openmp-*.bc"} ^ concat(clangBaseGlobs), ".cpp"}
  //
#endif
#ifdef FIXTURE_MICROSTREAM_CLANG_HIP_EXPR
      ,
      std::tuple{FIXTURE_MICROSTREAM_CLANG_HIP_EXPR, //
                 std::vector{"*/*main-host-*.bc", "*/*main-hip-*.bc"} ^ concat(clangBaseGlobs),
                 ".cpp"} //
#endif
#ifdef FIXTURE_MICROSTREAM_CLANG_CUDA_EXPR
      ,
      std::tuple{FIXTURE_MICROSTREAM_CLANG_CUDA_EXPR,
                 std::vector{"*/*main.bc", "*/*main-cuda-*-sm_60.bc"} ^ concat(clangBaseGlobs),
                 ".cpp"}
#endif
  ); //

  auto [name, compiler, model, dir] = expr;

  auto outDir = fmt::format("{}/{}_{}_{}_db", FIXTURE_TMP_DIR, name, compiler, model);
  INFO(outDir);

  DYNAMIC_SECTION(compiler << " " << model) {

    DYNAMIC_SECTION("index " << model) {
      int code = agv::index::run(agv::index::Options{
          .buildDir = dir,
          .sourceGlobs = {"*"},
          .outDir = outDir,
          .clearOutDir = true,
          .verbose = true,
          .maxThreads = static_cast<int>(std::thread::hardware_concurrency()),

      });
      CHECK(code == 0);

      try {
        std::vector<std::string> files;
        for (const auto &entry : std::filesystem::directory_iterator(outDir))
          files.emplace_back(entry.path());
        for (auto glob : globs) {
          auto matches = files ^ filter([&](auto file) {
                           return std::regex_match(file, agv::globToRegex(glob));
                         });
          INFO(glob << " matches one of " << (files ^ mk_string("[", ",", "]")));
          CHECK(matches.size() == 1);
        }
      } catch (const std::exception &e) {
        FAIL("cannot walk directory" << outDir << ": " << e.what());
      }
    }

    DYNAMIC_SECTION("inspect " << model) {
      auto buffer = std::cout.rdbuf();
      std::ostringstream ss;
      std::cout.rdbuf(ss.rdbuf());
      int code = agv::inspect::run(agv::inspect::Options{.dbDir = outDir});
      std::cout.rdbuf(buffer);
      CHECK(code == 0);
      auto actual = ss.str() ^ lines();
      REQUIRE(actual.size() == 2);
      CHECK(actual[0] == "entry,deps");
      CHECK(actual[1] ^ contains_slice(fmt::format("main{},", ext)));
    }

    DYNAMIC_SECTION("script " << model) {
      std::string scriptPath = fmt::format("{}/script.lua", FIXTURE_TMP_DIR);
      {
        std::ofstream script(scriptPath, std::ios::trunc);
        REQUIRE(script.is_open());
        script << R"(
print(#arg)
print(arg[1])
local db = Codebase.loadDB(arg[1])

local n = 0
for _ in pairs(db:entries()) do n = n + 1 end
print(n)

local cb = Codebase.load(db, true, arg[1], {}, function(s) return true end)
print(cb:units()[1]:name())
print(Diff.apted(cb:units()[1]:sTree(), cb:units()[1]:sTree()))
)";
      }
      auto buffer = std::cout.rdbuf();
      std::ostringstream captured;
      std::cout.rdbuf(captured.rdbuf());
      int code = agv::script::run(
          agv::script::Options{.roots = {},
                               .defs = false,
                               .noBuffer = false,
                               .maxThreads = static_cast<int>(std::thread::hardware_concurrency()),
                               .args = {scriptPath, outDir}});
      std::cout.rdbuf(buffer);

      CHECK(code == 0);

      auto actual =
          captured.str() ^ lines() ^ filter([](auto &l) { return !(l ^ starts_with("#")); });
      INFO((actual ^ mk_string("\n")));
      REQUIRE(actual.size() == 5);
      CHECK(actual[0] == "1");
      CHECK(actual[1] == outDir);
      CHECK(actual[2] == "1");
      CHECK(actual[3] ^ starts_with("main.")); // either main.cpp or main.ii
      CHECK(actual[4] == "0.0");
    }
  }
}