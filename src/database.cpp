#include "p3md/database.h"
#include "p3md/compress.h"

#include "clang/Frontend/ASTUnit.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"

#include "aspartame/map.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;
using namespace clang;

std::unique_ptr<p3md::Database> p3md::Database::fromJson(const std::string &json) {
  auto database = std::make_unique<p3md::Database>();
  nlohmann::json dbJson = nlohmann::json::parse(json);
  nlohmann::from_json(dbJson, *database);
  return database;
}

p3md::Database::Materialised::Materialised(const Database &db)
    : units(db.entries ^ map_values([&](auto &pch) -> std::unique_ptr<clang::ASTUnit> {
              IntrusiveRefCntPtr<DiagnosticOptions> opts = new DiagnosticOptions();
              auto diagnostics = CompilerInstance::createDiagnostics(opts.get());

              IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> vfs =
                  new llvm::vfs::InMemoryFileSystem();

              auto pchData = p3md::utils::zStdDecompress(pch.pchName);
              if (!pchData) {
                std::cerr << "Cannot read PCH data:" << pch.pchName << std::endl;
                return nullptr;
              }
              auto &backing = astBackingBuffer.emplace_back(*pchData);

              auto mb = llvm::MemoryBuffer::getMemBuffer(
                  llvm::StringRef(backing.data(), backing.size()), "", false);
              vfs->addFile(pch.pchName, 0, std::move(mb));
              for (auto &[name, actual] : pch.dependencies) {
                if (auto source = db.dependencies.find(actual); source != db.dependencies.end()) {
                  vfs->addFile(name, source->second.modified,
                               llvm::MemoryBuffer::getMemBuffer(source->second.content));
                } else {
                  std::cerr << "Cannot find dependency " << actual << " in db!" << std::endl;
                }
              }

              auto opt = std::make_shared<clang::HeaderSearchOptions>();
              return ASTUnit::LoadFromASTFile(pch.pchName,                      //
                                              clang::RawPCHContainerReader(),   //
                                              ASTUnit::WhatToLoad::LoadASTOnly, //
                                              diagnostics,                      //
                                              clang::FileSystemOptions(""),     //
                                              opt, false, true, CaptureDiagsKind::None, true, true,
                                              vfs);
            })) {}
