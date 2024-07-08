#include <iostream>

#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"
#include "p3md/database.h"
#include "p3md/list.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace aspartame;
using namespace clang;

int p3md::list::run(const p3md::list::Options &options) {
  auto dbDile = options.dbDir + "/db.json";
  auto buffer = llvm::MemoryBuffer::getFile(dbDile, /*IsText*/ true);
  if (auto e = buffer.getError()) {
    std::cerr << "Cannot read file " << dbDile << ": " << e.message() << std::endl;
    return e.value();
  }
  auto database = p3md::Database::fromJson((*buffer)->getBuffer().str());
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
