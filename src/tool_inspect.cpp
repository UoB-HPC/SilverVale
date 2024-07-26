#include <iostream>

#include "aspartame/vector.hpp"
#include "sv/cli.h"
#include "sv/model.h"
#include "sv/tool_inspect.h"

using namespace aspartame;
using namespace clang;
using namespace llvm;

static Expected<sv::inspect::Options> parseOpts(int argc, const char **argv) {
  static cl::OptionCategory category("List options");

  static cl::opt<std::string> dbPath(
      "db", cl::desc("The path to the P3MD database, as generated using the build command"),
      cl::Required, cl::cat(category));

  if (auto e = sv::parseCategory(category, argc, argv); e) return std::move(*e);
  return sv::inspect::Options{dbPath.getValue()};
}

int sv::inspect::main(int argc, const char **argv) {
  return parseAndRun(argc, argv, &parseOpts, &run);
}

int sv::inspect::run(const Options &options) {
  auto database = Codebase::loadDB(options.dbDir);
  std::cout << "entry,deps\n";
  database.entries //
      ^
      map([](auto &e) {                                                                           //
        return std::visit([](auto &x) { return std::pair{x->file, x->dependencies.size()}; }, e); //
      })                                                                                          //
      ^ sort_by([](auto &, auto deps) { return deps; })                                           //
      ^ for_each([](auto &file, auto deps) { std::cout << file << "," << deps << "\n"; });        //
  return EXIT_SUCCESS;
}
