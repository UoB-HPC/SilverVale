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
//#include "llvm/Support/Host.h"
#include "llvm/TargetParser/Host.h"


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
  std::string path{}, kind{}, triple{};
};

// Clean-room of https://clang.llvm.org/docs/ClangOffloadBundler.html
// We can't use clang::OffloadBundler::ListBundleIDsInFile because it writes to llvm::outs()
// and we can't easily capture the output when threaded.
struct ClangOffloadBundle {
  static constexpr char BundleMagic[] = "__CLANG_OFFLOAD_BUNDLE__";
  struct Entry {
    mutable std::shared_ptr<std::ifstream> stream;
    uint64_t offset{}, size{}, idLength{};
    std::string id{};

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
    if (!std::filesystem::copy_file(src, dest, std::filesystem::copy_options::overwrite_existing) ||
        e) {
      SV_WARNF("failed to copy BC {} to {}: {}", src, dest, e.message());
      return;
    }

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
    if (verbose) SV_INFOF("added BC: {} (kind={}, triple={})", destName, kind, triple);
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

static auto collectTrees(sv::NTree<sv::SNode> &sTree, sv::NTree<sv::SNode> &sTreeInlined,
                         ASTContext &context, clang::Decl *decl, bool normalise) {
  auto createTree = [&](const sv::ClangASTSemanticTreeVisitor::Option &option) {
    sv::NTree<sv::SNode> topLevel{sv::SNode{"toplevel", sv::Location{}}, {}};
    sv::ClangASTSemanticTreeVisitor(&topLevel, context, option).TraverseDecl(decl);
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

struct Trees {
  sv::NTree<sv::SNode>                            //
      sTree{{"root", sv::Location{}}, {}},        //
      sTreeNamed{{"root", sv::Location{}}, {}},   //
      sTreeInlined{{"root", sv::Location{}}, {}}, //
      sTreeInlinedNamed{{"root", sv::Location{}}, {}};
};

void run(const sv::uproot::Options &options, clang::CompilerInstance &CI, Trees &trees) {
  auto stem = std::filesystem::path(options.file).stem();
  auto dFile = options.wd / fmt::format("{}.d", stem);

  auto bitcodes =
      collectBitcodeFiles(options.verbose, options.prefix, stem, options.wd, options.dest);

  clang::LangOptions o = CI.getLangOpts();
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
  else SV_WARNF("Cannot determine precise language for {}", options.file);

  llvm::LLVMContext context;
  std::vector<std::string> treeFiles;
  auto dump = [&](auto Suffix, auto tree) {
    auto path = options.dest / fmt::format("{}.{}.{}", options.prefix, stem, Suffix);
    auto start = std::chrono::high_resolution_clock::now();
    sv::writePacked(path, tree);
    size_t nodes{};
    tree.postOrderWalk([&](auto &, auto) { nodes++; });
    auto end = std::chrono::high_resolution_clock::now();
    if (options.verbose)
      SV_INFOF("[uproot] Wrote {} nodes to {} in {}", nodes, path,
               std::chrono::duration_cast<std::chrono::milliseconds>(end - start));
    treeFiles.emplace_back(path.filename());
  };

  //  sv::par_for(
  //
  //      std::vector{std::tuple{
  //                      true,
  //                      sv::EntryUnnamedSTreeSuffix,        //
  //                      sv::EntryUnnamedSTreeInlinedSuffix, //
  //                      sv::EntryUnnamedIrTreeSuffix        //
  //
  //                  },
  //                  std::tuple{
  //                      false,
  //                      sv::EntryNamedSTreeSuffix,        //
  //                      sv::EntryNamedSTreeInlinedSuffix, //
  //                      sv::EntryNamedIrTreeSuffix        //
  //                  }},
  //      [&](auto t, auto ){
  //        auto [normalise, streeSuffix, streeInlinedSuffix, irTreeSuffix] = t;
  //        auto irTree = inflateIrTreeFromFiles(
  //            context, bitcodes ^ map([&](auto &bc) { return options.dest / bc.file; }),
  //            normalise);
  //        dump(streeSuffix, normalise ? trees.sTree : trees.sTreeNamed);
  //        dump(streeInlinedSuffix, normalise ? trees.sTreeInlined : trees.sTreeInlinedNamed);
  //        dump(irTreeSuffix, irTree);
  //      } );

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
        context, bitcodes ^ map([&](auto &bc) { return options.dest / bc.path; }), normalise);
    dump(streeSuffix, normalise ? trees.sTree : trees.sTreeNamed);
    dump(streeInlinedSuffix, normalise ? trees.sTreeInlined : trees.sTreeInlinedNamed);
    dump(irTreeSuffix, irTree);
  }

  sv::Database::Entry result{.language = language,
                             .path = {},
                             .command = {},
                             .preprocessedFile = {},
                             .dependencyFile = {},
                             .treeFiles = treeFiles,
                             .attributes = {{"bitcodes", bitcodes ^ map([](auto &bc) {
                                                           return fmt::format("{},{},{}", bc.path,
                                                                              bc.kind, bc.triple);
                                                         }) ^ mk_string(";")}}};
  sv::writePacked(options.dest / fmt::format("{}.{}.{}", options.prefix, stem, sv::EntrySuffix),
                  result);
}

namespace {

class STreeConsumer : public ASTConsumer {
  CompilerInstance &CI;
  Trees &trees;

public:
  STreeConsumer(CompilerInstance &CI, Trees &trees) : CI(CI), trees(trees) {}

  bool HandleTopLevelDecl(DeclGroupRef DG) override {
    for (auto decl : DG) { // XXX traversing decl is not thread safe
      collectTrees(trees.sTree, trees.sTreeInlined, CI.getASTContext(), decl, true);
      collectTrees(trees.sTreeNamed, trees.sTreeInlinedNamed, CI.getASTContext(), decl, false);
    }
    return true;
  }
};

class UprootAction : public PluginASTAction {
  Trees trees;

public:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, llvm::StringRef) override {
    return std::make_unique<STreeConsumer>(CI, trees);
  }

protected:
  void EndSourceFileAction() override {
    if (auto options = sv::uproot::parseEnv(); options) {
      run(*options, getCompilerInstance(), trees);
    } else SV_WARNF("Cannot parse env vars");
  }

  bool BeginInvocation(CompilerInstance &CI) override {
    // only kick in when compiling host code for the AST
    // TODO it might be interesting to also save the offload AST as well
    auto requestedArch = CI.getTarget().getTriple().getArchName();
    llvm::Triple hostTriple(llvm::sys::getDefaultTargetTriple());
    return requestedArch == hostTriple.getArchName();
  }

  bool ParseArgs(const CompilerInstance &CI, const std::vector<std::string> &) override {
    return true;
  }
};
} // namespace
static FrontendPluginRegistry::Add<UprootAction> Action("uproot", "");
