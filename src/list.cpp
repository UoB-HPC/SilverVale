#include <clang/Frontend/ASTUnit.h>
#include <fstream>
#include <iostream>
#include <zlib.h>

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "llvm/Support/MemoryBuffer.h"

#include "aspartame/unordered_map.hpp"
#include "aspartame/view.hpp"

#include "json.hpp"
#include "p3md/database.h"
#include "p3md/list.h"
#include "p3md/tree.h"

#include "zstd.h"

using namespace aspartame;
using namespace clang;

static std::optional<std::vector<char>> decompressGzipInMemory(const std::string &filename) {
  std::vector<char> data;
  gzFile file = gzopen(filename.c_str(), "rb");
  if (file == nullptr) {
    std::cerr << "Error: Unable to open file " << filename << std::endl;
    return std::nullopt;
  }

  std::array<char, 16 * 1024> buffer{};
  int bytesRead{};
  while ((bytesRead = gzread(file, buffer.data(), buffer.size() * sizeof(char))) > 0) {
    data.insert(data.end(), buffer.begin(), buffer.begin() + bytesRead);
  }
  if (bytesRead < 0) {
    int error{};
    std::cerr << "Cannot inflate " << filename << ": " << gzerror(file, &error) << std::endl;
    gzclose(file);
    return std::nullopt;
  }
  gzclose(file);
  return data;
}

static std::optional<std::vector<char>> decompress(const std::string &filename) {
  auto buffer = llvm::MemoryBuffer::getFile(filename);
  if (auto error = buffer.getError(); error) {
    std::cerr << error.message() << std::endl;
    return std::nullopt;
  }

  return std::vector<char>{
    (*buffer)->getBufferStart(), (*buffer)->getBufferEnd()
  };
  auto rSize = ZSTD_getFrameContentSize((*buffer)->getBufferStart(), (*buffer)->getBufferSize());
  if (rSize == ZSTD_CONTENTSIZE_ERROR) {
    std::cerr << "File not compressed by zstd! " << filename << std::endl;
    return std::nullopt;
  }
  if (rSize == ZSTD_CONTENTSIZE_UNKNOWN) {
    std::cerr << "File original size unknown: " << filename << std::endl;
    return std::nullopt;
  }

  std::vector<char> rBuff(rSize, 0);
  size_t dSize =
      ZSTD_decompress(rBuff.data(), rSize, (*buffer)->getBufferStart(), (*buffer)->getBufferSize());
  if (ZSTD_isError(dSize)) {
    std::cerr << ZSTD_getErrorName(dSize) << std::endl;
    return std::nullopt;
  }

  if (dSize != rSize) {
    std::cerr << "Impossible because zstd will check this condition!" << std::endl;
    return std::nullopt;
  }
  return rBuff;
}

int p3md::list ::run(const p3md::list::Options &options) {

  auto dbDile = options.dbDir + "/db.json";
  auto buffer = llvm::MemoryBuffer::getFile(dbDile, /*IsText*/ true);
  if (auto e = buffer.getError()) {
    std::cerr << "Cannot read file " << dbDile << ": " << e.message() << std::endl;
    return e.value();
  }

  nlohmann::json dbJson = nlohmann::json::parse((*buffer)->getBuffer().str());

  std::unordered_map<std::string, std::string> contentCache;
  p3md::Database database;
  nlohmann::from_json(dbJson, database);


  for (auto &[file, r] : database.entries) {

    std::cout << file << " " << r.pchName << " = \n";

    IntrusiveRefCntPtr<DiagnosticOptions> opts = new DiagnosticOptions();
    auto diagnostics = CompilerInstance::createDiagnostics(opts.get());

    IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> VFS = new llvm::vfs::InMemoryFileSystem();




    auto pchData = decompress(r.pchName);
    if (!pchData) { return EXIT_FAILURE; }

    std::cout << "@" << std::string(pchData->data(), pchData->data() + 4) << "\n";

    auto mb = llvm::MemoryBuffer::getMemBuffer(llvm::StringRef(pchData->data(), pchData->size()),
                                               "", false);
    VFS->addFile(r.pchName, 0, std::move(mb));
    for (auto &[name, actual] : r.dependencies) {
      if (auto source = database.dependencies ^ get(actual); source) {
        VFS->addFile(
            name, source->modified,
            llvm::MemoryBuffer::getMemBuffer(
                contentCache ^= get_or_emplace(actual, [&](auto) { return source->content; })));
      } else {
        std::cerr << "Cannot find dependency " << actual << " in db!" << std::endl;
      }
    }

    auto opt = std::make_shared<clang::HeaderSearchOptions>();
    std::unique_ptr<ASTUnit> that =
        ASTUnit::LoadFromASTFile((r.pchName),                      //
                                 clang::RawPCHContainerReader(),   //
                                 ASTUnit::WhatToLoad::LoadASTOnly, //
                                 diagnostics,                      //
                                 clang::FileSystemOptions(""),     //
                                 opt, false, true, CaptureDiagsKind::None, true, true, VFS);

    if (that) {
      std::cout << that << "\n";
      p3md::topLevelDeclsInMainFile(*that) | for_each([](auto x) { x->dump(); });
    }
  }

  return EXIT_SUCCESS;
}
