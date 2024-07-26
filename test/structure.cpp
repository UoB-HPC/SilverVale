#include <fstream>
#include <iostream>
#include <thread>
#include <filesystem>

#include "catch2/catch_test_macros.hpp"

#include "sv/model.h"
#include "sv/tool_index.h"
#include "sv/tool_inspect.h"
#include "sv/tool_script.h"
#include "fixture.h"


#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"

using namespace aspartame;

TEST_CASE("structure") {
  auto out = std::filesystem::path( FIXTURE_TMP_DIR) / "dummy_db";


  auto [name, compiler, model, dir] = FIXTURE_DUMMY_CLANG__EXPR;
  int code = sv::index::run(sv::index::Options{
      .buildDir = dir,
      .sourceGlobs = {"*"},
      .outDir = out,
      .coverageBin = "",
      .coverageRawDir = "",
      .coverageKind = sv::index::CoverageKind::AutoDetect,
      .clearOutDir = true,
      .verbose = true,
      .maxThreads = static_cast<int>(std::thread::hardware_concurrency()),

  });
  REQUIRE(code == 0);

  auto db = sv::Codebase::loadDB(out);
  auto cb = sv::Codebase::load(db, std::cout, true , {}, [](auto &) { return true; });
//
  REQUIRE(cb.units.size() == 2);
//
  auto main = cb.units[0];
//
//  std::cout << main->irTree().prettyPrint() << std::endl;
  std::cout << main->writtenSource(true).tsTree().prettyPrint() << std::endl;

  std::cout << main->irTree().prettyPrint() << std::endl;
}