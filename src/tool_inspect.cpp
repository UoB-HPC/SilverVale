#include <iostream>

#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"
#include "agv/cli.h"
#include "agv/database.h"
#include "agv/tool_inspect.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace aspartame;
using namespace clang;
using namespace llvm;

static Expected<agv::inspect::Options> parseOpts(int argc, const char **argv) {
  static cl::OptionCategory category("List options");

  static cl::opt<std::string> dbPath(
      "db", cl::desc("The path to the P3MD database, as generated using the build command"),
      cl::Required, cl::cat(category));

  static cl::opt<agv::inspect::Kind> kind(
      "kind", cl::desc("The kind of data to list"),
      cl::values(
          clEnumValN(agv::inspect::Kind::Entry, "entry", "Enable trivial optimizations"),
          clEnumValN(agv::inspect::Kind::Dependencies, "deps", "Enable default optimizations")),
      cl::Required, cl::cat(category));

  if (auto e = agv::parseCategory(category, argc, argv); e) return std::move(*e);
  return agv::inspect::Options{dbPath.getValue(), kind};
}

int agv::inspect::main(int argc, const char **argv) {
  return parseAndRun(argc, argv, &parseOpts, &run);
}

int agv::inspect::run(const Options &options) {
  auto dbDile = options.dbDir + "/db.json";
  auto buffer = llvm::MemoryBuffer::getFile(dbDile, /*IsText*/ true);
  if (auto e = buffer.getError()) {
    std::cerr << "Cannot read file " << dbDile << ": " << e.message() << std::endl;
    return e.value();
  }
  auto database = Database::fromJsonString((*buffer)->getBuffer().str());
  switch (options.kind) {
    case Kind::Entry:
      std::cout << "entry,deps\n";
      (database.entries | to_vector()) ^
          map([](auto &file, auto &pch) { return std::pair{file, pch.dependencies.size()}; }) ^
          sort_by([](auto &, auto deps) { return deps; }) ^
          for_each([](auto &file, auto deps) { std::cout << file << "," << deps << "\n"; });
      break;
    case Kind::Dependencies:
      std::cout << "dep,dependents,modified\n";
      (database.dependencies | keys() | map([&](auto &file) {
         auto rDeps = database.entries | values() |
                      count([&](auto &r) { return r.dependencies | values() | contains(file); });
         return std::pair{file, rDeps};
       }) |
       to_vector()) ^
          sort_by([](auto &, auto rDep) { return rDep; }) ^
          for_each([](auto &file, auto rDeps) { std::cout << file << "," << rDeps << "\n"; });
      break;
  }
  return EXIT_SUCCESS;
}
