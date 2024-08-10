#include <fstream>
#include <iostream>
#include <regex>

#include "compress.h"
#include "tree_clangast.h"
#include "tree_llvmir.h"

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/Driver/OffloadBundler.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"

#include "llvm/BinaryFormat/Magic.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/Module.h"
#include "llvm/Object/OffloadBinary.h"

#include "sv/cli.h"
#include "sv/database.h"
#include "sv/exec.h"
#include "sv/tree.h"
#include "sv/uproot.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace clang;
using namespace aspartame;

struct LLVMBitcode {
  std::string file{}, kind{}, triple{};
};

struct CPCHFile {
  static constexpr char Magic[] = "CPCH";

  template <typename OS> static bool isCPCH(OS &stream) {
    char magic[sizeof(Magic) - 1];
    return stream.read(magic, sizeof(magic)) && (std::strncmp(magic, Magic, sizeof(magic)) == 0);
  }
};

// Clean-room of https://clang.llvm.org/docs/ClangOffloadBundler.html
// We can't use clang::OffloadBundler::ListBundleIDsInFile because it writes to llvm::outs()
// and we can't easily capture the output when threaded.
struct ClangOffloadBundle {
  static constexpr char BundleMagic[] = "__CLANG_OFFLOAD_BUNDLE__";
  struct Entry {
    mutable std::shared_ptr<std::ifstream> stream;
    uint64_t offset, size, idLength;
    std::string id;

    [[nodiscard]] std::vector<char> read(size_t n) const {
      auto limit = std::min(size, n);
      std::vector<char> buffer(limit, '\0');
      stream->seekg(static_cast<ssize_t>(offset));
      stream->read(buffer.data(), static_cast<ssize_t>(limit));
      return buffer;
    }

    template <size_t bufferSize = 4096, typename OS> void readToStream(OS &destStream) const {
      stream->seekg(static_cast<ssize_t>(offset));
      std::array<char, bufferSize> buffer{};
      uint64_t remaining = size;
      while (remaining > 0) {
        stream->read(buffer.data(), std::min(bufferSize, remaining));
        auto count = stream->gcount();
        if (count == 0) break; // EOF
        else destStream.write(buffer.data(), count);
        remaining -= count;
      }
    }
  };
  std::vector<Entry> entries;
  static std::optional<Expected<ClangOffloadBundle>> parse(const std::filesystem::path &file) {
    auto fail = [](auto &&s) {
      return llvm::make_error<llvm::StringError>(s, llvm::inconvertibleErrorCode());
    };

    auto stream = std::make_shared<std::ifstream>(file, std::ios::binary);
    if (!*stream) return fail(fmt::format("Cannot open {}", file));

    char magic[sizeof(BundleMagic) - 1];
    if (!(stream->read(magic, sizeof(magic)) &&
          (std::strncmp(magic, BundleMagic, sizeof(magic)) == 0))) {
      return {};
    }

    auto read = [&]<typename T>(T *field, auto name) -> std::optional<std::string> {
      if (!stream->read(reinterpret_cast<char *>(field), sizeof(T))) {
        return (fmt::format("Cannot read {} bytes for ClangOffloadBundle.{}", sizeof(T), name));
      }
      return {};
    };
    ClangOffloadBundle bundle;
    uint64_t numEntries{};
    if (auto e = read(&numEntries, "numEntries"); e) return fail(*e);
    for (uint64_t i = 0; i < numEntries; ++i) {
      ClangOffloadBundle::Entry entry{.stream = stream};
      if (auto e = read(&entry.offset, "offset"); e) { return fail(*e); }
      if (auto e = read(&entry.size, "size"); e) { return fail(*e); }
      if (auto e = read(&entry.idLength, "idLength"); e) { return fail(*e); }
      entry.id.resize(entry.idLength);
      if (!stream->read(entry.id.data(), static_cast<std::streamsize>(entry.idLength))) {
        return fail(fmt::format("Cannot read {} bytes for ClangOffloadBundle.id", entry.idLength));
      } else bundle.entries.push_back(entry);
    }
    return bundle;
  }
};

// Handle TU.bc/TU-$triple.bc bitcode file
// -save-temps has the following convention for BC temps:
//   Host
//      {OUTPUT}.bc
//   CUDA/HIP
//     {OUTPUT}-host-{triple}.bc
//     {OUTPUT}-{cuda|hip}-{triple}.bc
//   OpenMP target:
//     {OUTPUT}-host-{triple}.bc
//     {OUTPUT}-openmp-{triple}.bc
// -emit-llvm uses the same format where:
//   Only CUDA will create a BC file,
//   HIP generates a clang-offload-bundle file
//   OpenMP target generates a normal BC with a 0x10ff10ad prefixed @llvm.embedded.object
//   see https://clang.llvm.org/docs/ClangOffloadPackager.html
static std::vector<LLVMBitcode> collectBitcodeFiles(bool verbose, const std::string &prefix,
                                                    const std::string &name,
                                                    const std::filesystem::path &wd,
                                                    const std::filesystem::path &dest) {
  std::vector<LLVMBitcode> codes;
  auto saveBC = [&, pattern = std::regex("^" + name + "-([a-zA-Z]+)-([a-zA-Z0-9-_]+)\\.bc$")](
                    const std::filesystem::path &src, const std::filesystem::path &dest) {
    auto e = std::error_code{};
    if (!std::filesystem::copy_file(src, dest) || e)
      SV_WARNF("failed to copy BC {} to {}: {}", src, dest, e.message());
    else {
      auto buffer = llvm::MemoryBuffer::getFile(src.string());
      if (!buffer) {
        SV_WARNF("error reading BC {}: {}", src, buffer.getError().message());
        return;
      }
      auto bufferRef = buffer->get()->getMemBufferRef();
      if (auto magic = identify_magic(bufferRef.getBuffer()); magic != llvm::file_magic::bitcode) {
        SV_WARNF("file {} is not a BC file (llvm::file_magic index={})", src,
                 static_cast<std::underlying_type_t<llvm::file_magic::Impl>>(magic));
        return;
      }

      SmallVector<llvm::object::OffloadFile> binaries;
      if (auto _ = llvm::object::extractOffloadBinaries(bufferRef, binaries)) {
        SV_WARNF("error reading embedded offload binaries for {}", src);
      }
      binaries | filter([](auto &f) {
        return f.getBinary()->getImageKind() == llvm::object::ImageKind::IMG_Bitcode;
      }) | for_each([&](auto &f) {
        auto kind = getOffloadKindName(f.getBinary()->getOffloadKind()).str();
        auto triple = f.getBinary()->getTriple().str();
        auto embeddedName = fmt::format("{}.{}-{}-{}.bc", prefix, name, kind, triple);
        if (verbose)
          SV_INFOF("adding embedded BC {} (kind={}, triple={})", embeddedName, kind, triple);

        std::ofstream file(dest.parent_path() / embeddedName, std::ios::binary);
        if (!file) {
          SV_WARNF("failed to write embedded offload binary {}: cannot open file for writing",
                   embeddedName);
        } else {
          file << f.getBinary()->getImage().str();
          codes.emplace_back(embeddedName, kind, triple);
        }
      });

      auto destName = dest.filename().string();
      std::smatch match;
      auto [_, kind, triple] = std::regex_match(destName, match, pattern)
                                   ? codes.emplace_back(destName, match[1].str(), match[2].str())
                                   : codes.emplace_back(destName, "host", "");
      if (verbose) SV_INFOF("adding BC: {} (kind={}, triple={})", destName, kind, triple);
    }
  };

  // first walk the wd to discover any existing target BC
  try {
    for (const auto &entry : std::filesystem::directory_iterator(wd)) {
      std::string bcFile = entry.path().filename();
      if (bcFile ^ starts_with(name + "-") && bcFile ^ ends_with(".bc")) {
        saveBC(bcFile, dest / fmt::format("{}.{}", prefix, bcFile));
      }
    }
  } catch (const std::exception &e) {
    SV_WARNF("failed to traverse working directory {} for BC files:{} ", wd, e);
  }

  // then handle the host BC itself
  std::string hostBCFile;
  if (auto bcFile = wd / fmt::format("{}.bc", name); std::filesystem::exists(bcFile))
    hostBCFile = bcFile; //
  else if (auto oFile = wd / fmt::format("{}.o", name); std::filesystem::exists(oFile))
    hostBCFile = oFile; //

  if (!hostBCFile.empty()) {
    // found a valid host BC, it could be a clang offload bundle: try to unbundle
    // The following drivers calls are equivalent to:
    //   clang-offload-bundler --list     --type bc --input $FILE
    //   clang-offload-bundler --unbundle --type bc --input $FILE --output $OUT --targets $TARGET
    std::vector<std::string> targets;
    auto bundle = ClangOffloadBundle::parse(hostBCFile);
    if (bundle) {
      if (auto e = bundle->takeError())
        SV_WARNF("cannot list offload bundles for {}: {}", hostBCFile, toString(std::move(e)));
      else {
        for (auto &entry : bundle->get().entries) {
          auto targetBCFile = fmt::format("{}.{}-{}.bc", prefix, name, entry.id);
          if (verbose) SV_INFOF("extracting {} from offload bundle {}", targetBCFile, hostBCFile);
          {
            std::ofstream stream(targetBCFile, std::ios::binary);
            entry.readToStream(stream);
          }
          saveBC(targetBCFile, dest / targetBCFile);
        }
      }
    } else {
      auto hostBCDest = dest / fmt::format("{}.{}.bc", prefix, name);
      saveBC(hostBCFile, hostBCDest);
    }
  }
  return codes;
}

static sv::NTree<sv::SNode>
inflateIrTreeFromFiles(llvm::LLVMContext &llvmContext,
                       const std::vector<std::filesystem::path> &bitcodeFiles, bool normalise) {
  auto children =
      bitcodeFiles //
      | collect([&](auto &bcFile)
                    -> std::optional<std::pair<std::string, std::shared_ptr<llvm::Module>>> {
          auto buffer = llvm::MemoryBuffer::getFile(bcFile.string());
          if (auto ec = buffer.getError()) {
            SV_WARNF("Cannot load BC file  {}: ", bcFile, ec);
            return {};
          }
          auto module = llvm::parseBitcodeFile(buffer.get()->getMemBufferRef(), llvmContext);
          if (auto e = module.takeError()) {
            SV_WARNF("Cannot parse BC file {}: ", bcFile, llvm::toString(std::move(e)));
            return {};
          }
          return std::pair{bcFile.string(), std::move(module.get())};
        }) //
      | map([&](auto &name, auto &module) {
          sv::NTree<sv::SNode> irTree{{normalise ? "(bc)" : name, sv::Location{}}, {}};
          sv::LLVMIRTreeVisitor(&irTree, *module, normalise);
          return irTree;
        }) //
      | to_vector();
  return {.value = {"root", sv::Location{}}, .children = children};
}

auto inflateASTFromFiles(const std::map<std::string, sv::Dependency> &dependencies,
                         const std::filesystem::path &pchFile) {
  llvm::IntrusiveRefCntPtr<clang::DiagnosticOptions> opts = new clang::DiagnosticOptions();
  auto diagnostics = clang::CompilerInstance::createDiagnostics(opts.get());
  llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> vfs = new llvm::vfs::InMemoryFileSystem();

  auto mb = llvm::MemoryBuffer::getFile(pchFile.string());
  if (auto e = mb.getError()) {
    SV_ERRF("Cannot read PCH data {}: {}", pchFile, e);
    std::exit(1);
  }
  vfs->addFile(pchFile.string(), 0, std::move(*mb));

  for (auto &[name, actual] : dependencies) {
    vfs->addFile(name, actual.modified, llvm::MemoryBuffer::getMemBuffer(actual.content));
  }
  auto ast = clang::ASTUnit::LoadFromASTFile(
      /*Filename*/ pchFile.string(),                      //
      /*PCHContainerRdr*/ clang::RawPCHContainerReader(), //
      /*ToLoad*/ clang::ASTUnit::WhatToLoad::LoadASTOnly, //
      /*Diags*/ diagnostics,                              //
      /*FileSystemOpts*/ clang::FileSystemOptions(""),    //

#if LLVM_VERSION_MAJOR < 17
      /*UseDebugInfo*/ false,
      /*OnlyLocalDecls*/ false,
#elif LLVM_VERSION_MAJOR < 18
      /*HSOpts*/ std::make_shared<clang::HeaderSearchOptions>(),
      /*UseDebugInfo*/ false,
      /*OnlyLocalDecls*/ false,
#elif LLVM_VERSION_MAJOR == 18
      /*HSOpts*/ std::make_shared<clang::HeaderSearchOptions>(),
      /*OnlyLocalDecls*/ false,
#elif LLVM_VERSION_MAJOR > 18
      /*HSOpts*/ std::make_shared<clang::HeaderSearchOptions>(),
      /*LangOpts*/ nullptr,
      /*OnlyLocalDecls*/ false,
#endif

      /*CaptureDiagnostics*/ clang::CaptureDiagsKind::None,
      /*AllowASTWithCompilerErrors*/ true, /*UserFilesAreVolatile*/ true, /*VFS*/ vfs);
  return ast;
}

auto inflateSTreeFromAST(ASTUnit &ast, bool normalise) {
  sv::NTree<sv::SNode> sTree{{"root", sv::Location{}}, {}};
  sv::NTree<sv::SNode> sTreeInlined{{"root", sv::Location{}}, {}};

  clang::SourceManager &sm = ast.getSourceManager();
  for (clang::Decl *decl :
       sv::topLevelDeclsInMainFile(ast) ^ sort_by([&](clang::Decl *decl) {
         return std::pair{sm.getDecomposedExpansionLoc(decl->getBeginLoc()).second,
                          sm.getDecomposedExpansionLoc(decl->getEndLoc()).second};
       })) {
    auto createTree = [&](const sv::ClangASTSemanticTreeVisitor::Option &option) {
      sv::NTree<sv::SNode> topLevel{sv::SNode{"toplevel", sv::Location{}}, {}};
      sv::ClangASTSemanticTreeVisitor(&topLevel, ast.getASTContext(), option).TraverseDecl(decl);
      return topLevel;
    };
    sTree.children.emplace_back(createTree({
        .inlineCalls = false,
        .normaliseVarName = normalise, //
        .normaliseFnName = normalise,  //
        .roots = {}                    //
    }));
    sTreeInlined.children.emplace_back(createTree({
        .inlineCalls = true,
        .normaliseVarName = normalise, //
        .normaliseFnName = normalise,  //
        .roots = {}                    //
    }));
  }
  return std::pair{sTree, sTreeInlined};
}

void saveCompressedPCH(const std::filesystem::path &pchFile, const std::filesystem::path &pchDest) {
  std::error_code zstdError;
  auto pchStream = sv::utils::zstd_ostream(pchDest, zstdError, 6);
  if (zstdError)
    SV_WARNF("cannot open compressed stream for PCH {}: {}", pchDest, zstdError.message());
  else {
    auto buffer = llvm::MemoryBuffer::getFile(pchFile.string());
    if (auto bufferError = buffer.getError())
      SV_WARNF("cannot open PCH {}: {}", pchFile, bufferError.message());
    else {
      auto ptr = std::move(buffer.get());
      (pchStream << ptr->getBuffer()).flush();
    }
  }
}

static bool extractPCH(const sv::uproot::Options &options, const std::filesystem::path &pchFile) {
  // It's possible the .pch file is actually a clang offload bundle with an ast entry.
  // This behaviour was observed Intel ICPX with -fsycl
  auto bundle = ClangOffloadBundle::parse(pchFile);
  if (bundle) {

    if (auto error = bundle->takeError()) {
      SV_WARNF("Cannot parse embedded PCH in clang offload bundle {}: {}", pchFile,
               llvm::toString(std::move(error)));
      return false;
    }

    auto embeddedPCHFiles =
        bundle->get().entries ^ collect([&](auto &entry) -> std::optional<std::filesystem::path> {
          auto magicBytes = entry.read(4);
          std::stringstream magicOnly(std::string(magicBytes.begin(), magicBytes.end()));
          if (CPCHFile::isCPCH(magicOnly)) {
            auto embeddedPCHFile =
                options.dest / fmt::format("{}-{}.pch", pchFile.stem(), entry.id);
            if (options.verbose)
              SV_INFOF("Extracted embedded PCH entry {} from {}", entry.id, pchFile);
            std::ofstream out(embeddedPCHFile, std::ios::binary);
            entry.readToStream(out);
            return embeddedPCHFile;
          }
          return std::nullopt;
        });

    if (embeddedPCHFiles.empty()) {
      SV_WARNF("No PCH entries found in offload bundle {}", embeddedPCHFiles ^ mk_string((",")),
               pchFile);
      return false;
    } else {
      if (embeddedPCHFiles.size() > 1) {
        SV_WARNF("More than one PCH entries ({}) found in offload bundle {}, only the first one "
                 "will be used",
                 embeddedPCHFiles ^ mk_string((",")), pchFile);
      }

      auto bundleName = fmt::format("{}.bundle", pchFile);
      std::filesystem::rename(pchFile, bundleName);
      std::filesystem::rename(embeddedPCHFiles[0], pchFile);
      if (options.verbose)
        SV_INFOF("Replaced original PCH {} (now {}) with {}", //
                 pchFile, bundleName, embeddedPCHFiles[0]);
    }
  } else { // it's not a clang offload bundle, but check if it's a valid PCH
           //    std::ifstream pchStream(pchFile, std::ios::binary);
           //    auto cpch = CPCHFile::isCPCH(pchStream);
           //    pchStream.close();
           //    if (!cpch) {
           //      SV_ERRF("PCH file expected but magic bytes mismatched: {}", pchFile);
           //      return false;
           //    }
  }
  return true;
}

int run(const sv::uproot::Options &options) {
  auto stem = std::filesystem::path(options.file).stem();
  auto pchFile = options.wd / fmt::format("{}.pch", stem);
  auto dFile = options.wd / fmt::format("{}.d", stem);

  if (!extractPCH(options, pchFile)) { return 1; }

  {
    std::ifstream iff(pchFile);
    if (!iff) {
      SV_ERRF("BAD {}", pchFile);
      return 1;
    }
  }

  auto pchDest = options.dest / fmt::format("{}.{}.zstd", options.prefix, pchFile.filename());
  saveCompressedPCH(pchFile, pchDest);
  if (options.verbose) SV_INFOF("Saved compressed PCH file to {}", pchDest);

  auto dependencies = sv::uproot::readDepFile(dFile, options.file);
  auto bitcodes =
      collectBitcodeFiles(options.verbose, options.prefix, stem, options.wd, options.dest);
  auto ast = inflateASTFromFiles(dependencies, pchFile);
  if (!ast) {
    SV_ERRF("AST from PHC file {} cannot be loaded", pchFile);
    return 1;
  }

  clang::LangOptions o = ast->getLangOpts();
  std::string language;
  // we consider HIP/CUDA the same, but HIP also sets CUDA and not the other way around
  if (o.HIP || o.CUDA) language = "cuda";
  else if (o.CPlusPlus) language = "cpp";

  else if (o.C99 || o.C11 || o.C17 ||
#if LLVM_VERSION_MAJOR >= 18
           o.C23
#else
           o.C2x
#endif
  )
    language = "c";
  else SV_WARNF("Cannot determine precise language from PCH for {}", pchFile);

  llvm::LLVMContext context;
  std::vector<std::string> treeFiles;
  auto dump = [&](auto Suffix, auto tree) {
    auto path = options.dest / fmt::format("{}.{}.{}", options.prefix, stem, Suffix);
    std::ofstream out(path, std::ios::out);
    if (!out) SV_WARNF("[uproot] Unable to open {} for writing", path);
    else {
      out << nlohmann::json(tree);
      size_t nodes{};
      tree.postOrderWalk([&](auto, auto) { nodes++; });
      if (options.verbose) SV_INFOF("[uproot] Wrote {} nodes to {}", nodes, path);
      treeFiles.emplace_back(path.filename());
    }
  };
  for (auto [normalise, streeSuffix, streeInlinedSuffix, irTreeSuffix] :
       {std::tuple{
            true,
            sv::EntryUnnamedSTreeSuffix,        //
            sv::EntryUnnamedSTreeInlinedSuffix, //
            sv::EntryUnnamedIrTreeSuffix        //

        },
        std::tuple{
            false,
            sv::EntryNamedSTreeSuffix,        //
            sv::EntryNamedSTreeInlinedSuffix, //
            sv::EntryNamedIrTreeSuffix        //
        }}) {

    auto irTree = inflateIrTreeFromFiles(
        context, bitcodes ^ map([&](auto &bc) { return options.dest / bc.file; }), normalise);
    auto [stree, streeInlined] = inflateSTreeFromAST(*ast, normalise);
    dump(streeSuffix, stree);
    dump(streeInlinedSuffix, streeInlined);
    dump(irTreeSuffix, irTree);
  }

  auto dependencyFile =
      options.dest / fmt::format("{}.{}.{}", options.prefix, stem, sv::EntryDepSuffix);
  sv::writeJSON(dependencyFile, dependencies);

  sv::Database::Entry result{.language = language,
                             .file = {},
                             .command = {},
                             .preprocessedFile = {},
                             .dependencyFile = dependencyFile,
                             .treeFiles = treeFiles,

                             .attributes = {{"pchFile", pchDest.string()},
                                            {"bitcodes", bitcodes ^ map([](auto &bc) {
                                                           return fmt::format("{},{},{}", bc.file,
                                                                              bc.kind, bc.triple);
                                                         }) ^ mk_string(";")}}};
  sv::writeJSON(options.dest / fmt::format("{}.{}.{}", options.prefix, stem, sv::EntrySuffix),
                result);
  return 0;
}

extern "C" int entry() {
  if (auto options = sv::uproot::parseEnv(); options) {
    return run(*options);
  } else SV_WARNF("Cannot parse env vars");
  return 1;
}

namespace {
class UprootAction : public PluginASTAction {
public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &, llvm::StringRef) override {
    return {};
  }

  bool ParseArgs(const CompilerInstance &, const std::vector<std::string> &args) override {
    return true;
  }

protected:
  bool PrepareToExecuteAction(CompilerInstance &) override {
    std::cout << "Hey!" <<std::endl;
    std::exit(entry()); }

public:
  PluginASTAction::ActionType getActionType() override { return ReplaceAction; }
};
} // namespace
static FrontendPluginRegistry::Add<UprootAction> Action("uproot", "");
