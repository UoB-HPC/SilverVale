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
using namespace llvm;
using namespace aspartame;

struct LLVMBitcode {
  std::string file{}, kind{}, triple{};
};

// Clean-room of https://clang.llvm.org/docs/ClangOffloadBundler.html
// We can't use clang::OffloadBundler::ListBundleIDsInFile because it writes to llvm::outs()
// and we can't easily capture the output when threaded.
struct ClangOffloadBundle {
  static constexpr char BundleMagic[] = "__CLANG_OFFLOAD_BUNDLE__";
  struct Entry {
    uint64_t offset, size, idLength;
    std::string id;
  };
  std::vector<Entry> entries;
  static std::optional<Expected<ClangOffloadBundle>> parse(const std::string &filename) {
    auto fail = [](auto &&s) { return make_error<StringError>(s, inconvertibleErrorCode()); };
    std::ifstream file(filename, std::ios::binary);
    if (!file) return fail(fmt::format("Cannot open {}", filename));

    char magic[sizeof(BundleMagic) - 1];
    if (!(file.read(magic, sizeof(magic)) &&
          (std::strncmp(magic, BundleMagic, sizeof(magic)) == 0))) {
      return {};
    }

    auto read = [&]<typename T>(T *field, auto name) -> std::optional<std::string> {
      if (!file.read(reinterpret_cast<char *>(field), sizeof(T))) {
        return (fmt::format("Cannot read {} bytes for ClangOffloadBundle.{}", sizeof(T), name));
      }
      return {};
    };
    ClangOffloadBundle bundle;
    uint64_t numEntries{};
    if (auto e = read(&numEntries, "numEntries"); e) return fail(*e);
    for (uint64_t i = 0; i < numEntries; ++i) {
      ClangOffloadBundle::Entry entry{};
      if (auto e = read(&entry.offset, "offset"); e) { return fail(*e); }
      if (auto e = read(&entry.size, "size"); e) { return fail(*e); }
      if (auto e = read(&entry.idLength, "idLength"); e) { return fail(*e); }
      entry.id.resize(entry.idLength);
      if (!file.read(entry.id.data(), static_cast<std::streamsize>(entry.idLength))) {
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
      auto buffer = MemoryBuffer::getFile(src.string());
      if (!buffer) {
        SV_WARNF("error reading BC {}: {}", src, buffer.getError().message());
        return;
      }
      auto bufferRef = buffer->get()->getMemBufferRef();
      if (auto magic = identify_magic(bufferRef.getBuffer()); magic != file_magic::bitcode) {
        SV_WARNF("file {} is not a BC file (llvm::file_magic index={})", src,
                 static_cast<std::underlying_type_t<file_magic::Impl>>(magic));
        return;
      }

      SmallVector<object::OffloadFile> binaries;
      if (auto _ = object::extractOffloadBinaries(bufferRef, binaries)) {
        SV_WARNF("error reading embedded offload binaries for {}", src);
      }
      binaries | filter([](auto &f) {
        return f.getBinary()->getImageKind() == object::ImageKind::IMG_Bitcode;
      }) | for_each([&](auto &f) {
        auto kind = getOffloadKindName(f.getBinary()->getOffloadKind()).str();
        auto triple = f.getBinary()->getTriple().str();
        auto embeddedName = fmt::format("{}.{}-{}-{}.bc", prefix, name, kind, triple);
        if (verbose)
          SV_INFOF("adding embedded BC {} (kind={}, triple={})", embeddedName, kind, triple);
        std::error_code embeddedEC;
        llvm::raw_fd_ostream file((dest.parent_path() / embeddedName).string(), embeddedEC);
        file << f.getBinary()->getImage();
        if (embeddedEC) {
          SV_WARNF("failed to write embedded offload binary {}: {}", embeddedName,
                   embeddedEC.message());
        } else codes.emplace_back(embeddedName, kind, triple);
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
      else targets = bundle->get().entries ^ map([](auto x) { return x.id; });
    }
    auto extracted = targets ^ collect([&](auto &target) -> std::optional<std::string> {
                       auto targetBCFile = fmt::format("{}.{}-{}.bc", prefix, name, target);
                       clang::OffloadBundlerConfig config;
                       config.FilesType = "bc";
                       config.ObjcopyPath = "";
                       config.InputFileNames = {hostBCFile};
                       config.OutputFileNames = {targetBCFile};
                       config.TargetNames = {target};
                       if (auto e = clang::OffloadBundler(config).UnbundleFiles()) {
                         SV_WARNF("cannot extract target {} from {}", target, hostBCFile);
                         return std::nullopt;
                       }
                       if (verbose)
                         SV_INFOF("extracted {} from offload bundle {}", targetBCFile, hostBCFile);
                       return {targetBCFile};
                     });
    auto hostBCDest = dest / fmt::format("{}.{}.bc", prefix, name);
    if (targets.empty()) saveBC(hostBCFile, hostBCDest); // not an offload bundle, copy the host BC
    else {
      if (extracted.size() != targets.size()) {
        SV_WARNF(
            "not all BC extracted, got [{}] targets but extracted only [{}], retaining all BCs",
            targets | mk_string(","), extracted | mk_string(","));
        saveBC(hostBCFile, hostBCDest);
      }
      for (auto &file : extracted) { // copy the extracted targets then delete
        saveBC(file, dest / file);
        if (!std::filesystem::remove(file)) SV_WARNF("cannot remove extracted temporary {}", file);
      }
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
    SV_WARNF("Cannot read PCH data {}: {}", pchFile, e);
    //    return {nullptr, modules};
  }
  vfs->addFile(pchFile.string(), 0, std::move(*mb));

  for (auto &[name, actual] : dependencies) {
    vfs->addFile(name, actual.modified, llvm::MemoryBuffer::getMemBuffer(actual.content));
  }

  auto opt = std::make_shared<clang::HeaderSearchOptions>();
  auto ast = clang::ASTUnit::LoadFromASTFile(pchFile,                                 //
                                             clang::RawPCHContainerReader(),          //
                                             clang::ASTUnit::WhatToLoad::LoadASTOnly, //
                                             diagnostics,                             //
                                             clang::FileSystemOptions(""),            //
                                             opt, false,
#if LLVM_VERSION_MAJOR < 18
                                             true,
#endif
                                             clang::CaptureDiagsKind::None, true, true, vfs);
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

auto collectFiles(const sv::uproot::Options &options) {
  auto stem = std::filesystem::path(options.file).stem();
  auto pchFile = options.wd / fmt::format("{}.pch", stem);
  auto dFile = options.wd / fmt::format("{}.d", stem);
  auto pchDest = options.dest / fmt::format("{}.{}.zstd", options.prefix, pchFile.filename());
  { // handle TU.pch CPCH file
    std::error_code zstdError;
    auto pchStream = sv::utils::zstd_ostream(pchDest, zstdError, 6);
    if (zstdError)
      SV_WARNF("cannot open compressed stream for PHC {}: {}", pchDest, zstdError.message());
    else {
      auto buffer = MemoryBuffer::getFile(pchFile.string());
      if (auto bufferError = buffer.getError())
        SV_WARNF("cannot open PCH {}: {}", pchFile, bufferError.message());
      else {
        auto ptr = std::move(buffer.get());
        (pchStream << ptr->getBuffer()).flush();
      }
    }
  }

  auto dependencies = sv::uproot::readDepFile(dFile, options.file);
  auto bitcodes =
      collectBitcodeFiles(options.verbose, options.prefix, stem, options.wd, options.dest);

  auto ast = inflateASTFromFiles(dependencies, pchFile);
  if (!ast) {
    SV_ERRF("AST from PHC file {} cannot be loaded", pchFile);
    std::exit(1);
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
      SV_INFOF("[uproot] Wrote {} nodes to {}", nodes, path);
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
}

extern "C" int entry() {
  if (auto options = sv::uproot::parseEnv(); options) {
    collectFiles(*options);
    return 0;
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
  bool PrepareToExecuteAction(CompilerInstance &) override { std::exit(entry()); }

public:
  PluginASTAction::ActionType getActionType() override { return ReplaceAction; }
};
} // namespace
static FrontendPluginRegistry::Add<UprootAction> Action("uproot", "");
