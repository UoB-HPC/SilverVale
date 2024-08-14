#include <filesystem>
#include <fstream>
#include <iostream>
#include <thread>

#include "catch2/catch_test_macros.hpp"
#include "catch2/generators/catch_generators.hpp"

#include "fixture.h"
#include "fmt/core.h"
#include "sv/exec.h"
#include "sv/model.h"
#include "sv/tool_dump.h"
#include "sv/tool_index.h"

#include "aspartame/set.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;
using namespace sv;

TEST_CASE("simple") {

  auto [name, compiler, variant, dir] = //
      GENERATE_REF(                     //
          FIXTURE_SIMPLE_CLANG_CXX_EXPR //
                                        //          FIXTURE_SIMPLE_GCC_CXX_EXPR,   //
                                        //          FIXTURE_SIMPLE_GCC_F90_EXPR
      );                                //

  auto outDir = fmt::format("{}/{}_{}_db", FIXTURE_TMP_DIR, name, compiler);
  INFO(outDir);

  DYNAMIC_SECTION(compiler) {
    auto exe = std::filesystem::path(dir) / name;
    DYNAMIC_SECTION("exec-" << name << "-" << variant) {
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
        auto code = exec(std::filesystem::path(dir) / name, std::cout);
        REQUIRE(code);
        CHECK(*code == 0);
      }
      std::filesystem::current_path(wd);
    }

    int code = index::run(index::Options{
        .buildDir = dir,
        .includeGlobs = {"*"},
        .excludeGlobs = {},
        .outDir = outDir,
        .coverageBin = exe,
        .coverageRawDir = "",
        .coverageKind = index::CoverageKind::AutoDetect,
        .clearOutDir = true,
        .verbose = true,
        .maxThreads = static_cast<int>(std::thread::hardware_concurrency()),
    });
    REQUIRE(code == 0);
    auto db = Codebase::loadDB(outDir);
    auto cb = Codebase::load(db, true, {}, //
                             [](auto &) { return true; });
    REQUIRE(cb.units.size() == 2);

    for (auto unit : cb.units) {
      // TODO implement assertions

      if (unit->name() ^ starts_with("main")) {

        std::cout << "# " << unit->name() << "\n";

        std::cout << (unit->sourceAsWritten().contentWhitespaceNormalised() ^ mk_string("\n")) << std::endl;

        std::cout << "STree" << std::endl;
        std::cout << unit->sTree(Unit::View::AsIs).prettyPrint() << std::endl;
        std::cout << unit->sTree(Unit::View::WithCov).prettyPrint() << std::endl;

        std::cout << "IRTREE" << std::endl;
        std::cout << unit->irTree(Unit::View::AsIs).prettyPrint() << std::endl;
        std::cout << unit->irTree(Unit::View::WithCov).prettyPrint() << std::endl;

        std::cout << "SRC" << std::endl;
        std::cout << unit->sourceAsWritten().tsTree().prettyPrint() << std::endl;
        std::cout << unit->sourcePreprocessed().tsTree().prettyPrint() << std::endl;
        std::cout << unit->sourceWithCoverage().tsTree().prettyPrint() << std::endl;
        std::cout << (unit->sourceWithCoverage().content() ^ mk_string("\n"))<< std::endl;
      }

      //      std::cout << "sourceAsWritten  \n";
      //      std::cout << (unit->sourceAsWritten().content() //
      //                    ^ lines()                         //
      //                    ^ zip_with_index(1)               //
      //                    ^ mk_string("\n", [](auto l, auto i) { return fmt::format("{:2}│{}", i,
      //                    l); }))
      //                << std::endl;
      //      std::cout << "sourcePreprocessed  \n";
      //      std::cout << (unit->sourcePreprocessed().content() //
      //                    ^ lines()                            //
      //                    ^ zip_with_index(1)                  //
      //                    ^ mk_string("\n", [](auto l, auto i) { return fmt::format("{:2}│{}", i,
      //                    l); }))
      //                << std::endl;
      //      std::cout << "sourceWithCoverage  \n";
      //      std::cout << (unit->sourceWithCoverage().content() //
      //                    ^ lines()                            //
      //                    ^ zip_with_index(1)                  //
      //                    ^ mk_string("\n", [](auto l, auto i) { return fmt::format("{:2}│{}", i,
      //                    l); }))
      //                << std::endl;

      //            std::cout << "asis\n";
      //            std::cout << unit->sourcePreprocessed(Unit::View::AsIs).tsTree().prettyPrint()
      //            << std::endl; std::cout << "cov\n"; std::cout <<
      //            unit->sourcePreprocessed(Unit::View::WithCoverage).tsTree().prettyPrint() <<
      //            std::endl;
    }

    //  std::cout << main->writtenSource(true).tsTree().prettyPrint() << std::endl;
    //
  }
}