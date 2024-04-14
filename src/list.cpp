#include <clang/Frontend/ASTUnit.h>
#include <fstream>
#include <iostream>
#include <zlib.h>

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "llvm/Support/MemoryBuffer.h"

#include "aspartame/unordered_map.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "json.hpp"
#include "p3md/compress.h"
#include "p3md/database.h"
#include "p3md/list.h"
#include "p3md/tree.h"

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
      (database->entries | to_vector()) ^ map([](auto &file, auto &pch) {
        return std::pair{file, pch.dependencies.size()};
      }) ^ sort_by([](auto &, auto deps) { return deps; }) ^
          for_each([](auto &file, auto deps) { std::cout << file << "," << deps << "\n"; });
      return EXIT_SUCCESS;
    case Kind::Dependencies:
      std::cout << "dep,dependents,modified\n";
      (database->dependencies | keys() | map([&](auto &file) {
         auto rDeps = database->entries | values() |
                      count([&](auto &r) { return r.dependencies | values() | contains(file); });
         return std::pair{file, rDeps};
       }) |
       to_vector()) ^
          sort_by([](auto &, auto rDep) { return rDep; }) ^
          for_each([](auto &file, auto rDeps) { std::cout << file << "," << rDeps << "\n"; });
      return EXIT_SUCCESS;
  }

  std::unordered_map<std::string, std::string> contentCache;

  //    std::cout << file << " " << r.pchName << " = \n";
  //
  //    IntrusiveRefCntPtr<DiagnosticOptions> opts = new DiagnosticOptions();
  //    auto diagnostics = CompilerInstance::createDiagnostics(opts.get());
  //
  //    IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> VFS = new
  //    llvm::vfs::InMemoryFileSystem();
  //
  //    auto pchData = p3md::utils::zstdDecompress(r.pchName);
  //    if (!pchData) { return EXIT_FAILURE; }
  //
  //    std::cout << "@" << std::string(pchData->data(), pchData->data() + 4) << "\n";
  //
  //    auto mb = llvm::MemoryBuffer::getMemBuffer(llvm::StringRef(pchData->data(),
  //    pchData->size()),
  //                                               "", false);
  //    VFS->addFile(r.pchName, 0, std::move(mb));
  //    for (auto &[name, actual] : r.dependencies) {
  //      if (auto source = database.dependencies ^ get(actual); source) {
  //        VFS->addFile(
  //            name, source->modified,
  //            llvm::MemoryBuffer::getMemBuffer(
  //                contentCache ^= get_or_emplace(actual, [&](auto) { return source->content;
  //                })));
  //      } else {
  //        std::cerr << "Cannot find dependency " << actual << " in db!" << std::endl;
  //      }
  //    }
  //
  //    auto opt = std::make_shared<clang::HeaderSearchOptions>();
  //    std::unique_ptr<ASTUnit> that =
  //        ASTUnit::LoadFromASTFile((r.pchName),                      //
  //                                 clang::RawPCHContainerReader(),   //
  //                                 ASTUnit::WhatToLoad::LoadASTOnly, //
  //                                 diagnostics,                      //
  //                                 clang::FileSystemOptions(""),     //
  //                                 opt, false, true, CaptureDiagsKind::None, true, true, VFS);
  //
  //    if (that) {
  //      std::cout << that << "\n";
  //      p3md::topLevelDeclsInMainFile(*that) | for_each([](auto x) { x->dump(); });
  //    }
}
