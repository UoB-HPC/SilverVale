#include <fstream>
#include <iostream>
#include <thread>

#include "catch2/catch_test_macros.hpp"

#include "agv/tool_index.h"
#include "agv/tool_inspect.h"
#include "agv/tool_script.h"
#include "agv/database.h"
#include "fixture.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"

using namespace aspartame;


TEST_CASE("structure") {
  auto out = std::string(FIXTURE_TMP_DIR) + "/dummy_db";

  int code = agv::index::run(agv::index::Options{
      .buildDir = FIXTURE_DUMMY__DIR,
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
  REQUIRE(code == 0);


  auto db = agv::Database::fromJsonFile(out+"/db.json");
  auto cb = db.load(std::cout, false, out, {}, [](auto &) {return true;});

  REQUIRE(cb.units.size() == 1);

  auto main = cb.units[0];

  std::cout <<
  main->irTree().prettyPrint() <<std::endl;




}