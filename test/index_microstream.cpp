#include <fstream>
#include <iostream>
#include <thread>
#include <tuple>

#include "catch2/catch_test_macros.hpp"
#include "catch2/generators/catch_generators.hpp"

#include "fixture.h"
#include "fmt/core.h"
#include "sv/diff.h"
#include "sv/glob.h"
#include "sv/model.h"
#include "sv/tool_dump.h"
#include "sv/tool_index.h"
#include "sv/tool_script.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "sv/exec.h"

using namespace aspartame;
using namespace sv;

TEST_CASE("microstream") {

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

  DYNAMIC_SECTION(compiler << "-" << model) {
    DYNAMIC_SECTION("exec-" << model) {
      auto wd = std::filesystem::current_path();
      std::filesystem::current_path(dir);

      for (const auto &entry : std::filesystem::recursive_directory_iterator(dir)) {
        if (entry.path().extension() == ".gcda") {
          std::cout << "Deleting residual GCov profile: " << entry.path() << std::endl;
          std::filesystem::remove(entry);
        }
      }
      {
        REQUIRE(setenv("CCACHE_DISABLE", "1", true) == 0);
        auto code =
            exec("cmake --build . --target clean && cmake --build . --target all", std::cout);
        REQUIRE(code);
        CHECK(*code == 0);
      }
      {
        auto code = exec(fmt::format("{}/{} 8192 10", dir, name), std::cout);
        REQUIRE(code);
        CHECK(*code == 0);
      }
      std::filesystem::current_path(wd);
    }

    DYNAMIC_SECTION("index-" << model) {
      int code = index::run(index::Options{
          .buildDir = dir,
          .sourceGlobs = {"*"},
          .outDir = outDir,
          .coverageBin = fmt::format("{}/{}", dir, name),
          .coverageRawDir = "",
          .coverageKind = index::CoverageKind::AutoDetect,
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
          auto matches =
              files ^ filter([&](auto file) { return std::regex_match(file, globToRegex(glob)); });
          INFO(glob << " matches one of " << (files ^ mk_string("[", ",", "]")));
          CHECK(matches.size() == 1);
        }
      } catch (const std::exception &e) {
        FAIL("cannot walk directory" << outDir << ": " << e.what());
      }
    }

    DYNAMIC_SECTION("dump-" << model) {
      auto buffer = std::cout.rdbuf();
      std::ostringstream ss;
      std::cout.rdbuf(ss.rdbuf());
      int code = dump::run(dump::Options{.dbDir = outDir});
      std::cout.rdbuf(buffer);
      CHECK(code == 0);
      auto actual = ss.str() ^ lines() ^ filter([](auto x) { return !(x ^ starts_with("# ")); });
      INFO(ss.str());
      REQUIRE(actual.size() == (1 + 2));
      CHECK(actual[0] ^ starts_with("entry,"));

      for (auto file : {fmt::format("main{},", ext), fmt::format("consume{},", ext)}) {
        CHECK(actual ^ exists([&](auto s) { return s ^ contains_slice(file); }));
      }
    }

    DYNAMIC_SECTION("load-" << model) {

      auto normalise = GENERATE(true, false);

      DYNAMIC_SECTION("load-" << model << "-" << (normalise ? "normalised" : "as-is")) {

        auto db = Codebase::loadDB(outDir);
        CHECK(db.entries.size() == 2);
        auto cb = Codebase::load(db, normalise, {}, [](auto) { return true; });

        REQUIRE(!cb.units.empty());

        for (auto file : {"main.", "consume."}) {
          CHECK(cb.units ^ exists([&](auto n) { return n->name() ^ starts_with(file); }));
        }

        for (auto &unit : cb.units) {
          // make sure it's not all empty trees with one root
          CHECK(unit->sTree(Unit::View::AsIs).maxWidth() > 1);
          CHECK(unit->sTreeInlined(Unit::View::AsIs).maxWidth() > 1);
          CHECK(unit->irTree(Unit::View::AsIs).maxWidth() > 1);
          CHECK(unit->sourceAsWritten().tsTree().maxWidth() > 1);
          CHECK(unit->sourcePreprocessed().tsTree().maxWidth() > 1);
          CHECK(unit->sourceWithCoverage().tsTree().maxWidth() > 1);

          // smoke test identity distance


          CHECK(Diff::apted(unit->sTree(Unit::View::AsIs), unit->sTree(Unit::View::AsIs)) == 0);
          // TODO parallelise, too slow
//          CHECK(Diff::apted(unit->sTreeInlined(Unit::View::AsIs),
//                            unit->sTreeInlined(Unit::View::AsIs)) == 0);
//          CHECK(Diff::apted(unit->irTree(Unit::View::AsIs), unit->irTree(Unit::View::AsIs)) == 0);
//          CHECK(Diff::apted(unit->sourceAsWritten().tsTree(), unit->sourceAsWritten().tsTree()) ==
//                0);
//          CHECK(Diff::apted(unit->sourcePreprocessed().tsTree(),
//                            unit->sourcePreprocessed().tsTree()) == 0);
//          CHECK(Diff::apted(unit->sourceWithCoverage().tsTree(),
//                            unit->sourceWithCoverage().tsTree()) == 0);

          // make sure processed TS is successful
          auto nodes = unit->sourcePreprocessed().tsTree().root    //
                       | map([](auto &n) { return n.value.data; }) //
                       | to_vector();
          CHECK(!nodes.empty());
          CHECK(!(nodes ^ contains("ERROR")));
        }
      }
    }

    DYNAMIC_SECTION("script-" << model) {
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

local cb = Codebase.load(db, true, {}, function(s) return true end)
for _, x in ipairs(cb:units()) do
  print(Diff.apted(x:sTree(0), x:sTree(0)) .. "," .. x:name())
end

)";
      }
      auto buffer = std::cout.rdbuf();
      std::ostringstream captured;
      std::cout.rdbuf(captured.rdbuf());
      int code = script::run(
          script::Options{.roots = {},
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
      CHECK(actual[2] == "2");
      for (auto file : {"main.", "consume."}) {
        CHECK(actual ^
              exists([&](auto x) { return x ^ starts_with(fmt::format("0.0,{}", file)); }));
      }
    }
  }
}