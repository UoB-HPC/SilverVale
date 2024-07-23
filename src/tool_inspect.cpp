#include <iostream>

#include "agv/cli.h"
#include "agv/model.h"
#include "agv/tool_inspect.h"
#include "aspartame/vector.hpp"

using namespace aspartame;
using namespace clang;
using namespace llvm;

static Expected<agv::inspect::Options> parseOpts(int argc, const char **argv) {
  static cl::OptionCategory category("List options");

  static cl::opt<std::string> dbPath(
      "db", cl::desc("The path to the P3MD database, as generated using the build command"),
      cl::Required, cl::cat(category));

  if (auto e = agv::parseCategory(category, argc, argv); e) return std::move(*e);
  return agv::inspect::Options{dbPath.getValue()};
}

int agv::inspect::main(int argc, const char **argv) {
  return parseAndRun(argc, argv, &parseOpts, &run);
}

int agv::inspect::run(const Options &options) {
  auto database = Codebase::loadDB(options.dbDir);
  std::cout << "entry,deps\n";
  database.entries                                                                                //
      ^ map([](auto &e) {                                                                         //
          return std::visit([](auto &x) { return std::pair{x.file, x.dependencies.size()}; }, e); //
        })                                                                                        //
      ^ sort_by([](auto &, auto deps) { return deps; })                                           //
      ^ for_each([](auto &file, auto deps) { std::cout << file << "," << deps << "\n"; });        //
  return EXIT_SUCCESS;
}
