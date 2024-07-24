#include <fstream>
#include <iostream>
#include <utility>

#include "sv/cli.h"
#include "sv/compress.h"
#include "sv/model.h"
#include "sv/par.h"
#include "sv/semantic_llvm.h"
#include "sv/semantic_ts.h"

#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "tree_sitter_c/api.h"
#include "tree_sitter_cpp/api.h"
#include "tree_sitter_cuda/api.h"
#include "tree_sitter_fortran/api.h"
#include "tree_sitter_julia/api.h"
#include "tree_sitter_rust/api.h"

#include "aspartame/map.hpp"
#include "aspartame/optional.hpp"
#include "aspartame/set.hpp"
#include "aspartame/string.hpp"
#include "aspartame/unordered_map.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;
using namespace clang;

class ClangContext {
  llvm::LLVMContext context;
  std::vector<std::vector<char>> astBackingBuffer;

  using EntryType = std::pair<std::shared_ptr<clang::ASTUnit>,
                              std::map<std::string, std::shared_ptr<llvm::Module>>>;

  static EntryType mkEntry(llvm::LLVMContext &llvmContext,          //
                           std::vector<std::vector<char>> &storage, //
                           const std::filesystem::path &baseDir,    //
                           const sv::ClangEntry &tu) {
    auto modules =
        tu.bitcodes |
        collect([&](auto &entry)
                    -> std::optional<std::pair<std::string, std::shared_ptr<llvm::Module>>> {
          auto bcFile = baseDir / entry.file;
          auto buffer = llvm::MemoryBuffer::getFile(bcFile.string());
          if (auto ec = buffer.getError()) {
            AGV_WARNF("Cannot load BC file  {}: ", bcFile, ec);
            return {};
          }

          auto module = llvm::parseBitcodeFile(buffer.get()->getMemBufferRef(), llvmContext);
          if (auto e = module.takeError()) {
            AGV_WARNF("Cannot parse BC file {}: ", bcFile, toString(std::move(e)));
            return {};
          }

          return std::pair{entry.file, std::move(module.get())};
        }) |
        and_then([](auto &xs) { return std::map{xs.begin(), xs.end()}; });

    IntrusiveRefCntPtr<DiagnosticOptions> opts = new DiagnosticOptions();
    auto diagnostics = CompilerInstance::createDiagnostics(opts.get());

    IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> vfs = new llvm::vfs::InMemoryFileSystem();

    auto pchFile = baseDir / tu.pchFile;

    auto pchData = sv::utils::zStdDecompress(pchFile);
    if (!pchData) {
      AGV_WARNF("Cannot read PCH data: {}", pchFile);
      return {nullptr, modules};
    }
    auto &backing = storage.emplace_back(*pchData);
    auto mb = llvm::MemoryBuffer::getMemBuffer(llvm::StringRef(backing.data(), backing.size()), "",
                                               false);
    vfs->addFile(pchFile.string(), 0, std::move(mb));
    for (auto &[name, actual] : tu.dependencies) {
      vfs->addFile(name, actual.modified, llvm::MemoryBuffer::getMemBuffer(actual.content));
    }

    auto opt = std::make_shared<clang::HeaderSearchOptions>();
    auto ast = ASTUnit::LoadFromASTFile(pchFile,                          //
                                        clang::RawPCHContainerReader(),   //
                                        ASTUnit::WhatToLoad::LoadASTOnly, //
                                        diagnostics,                      //
                                        clang::FileSystemOptions(""),     //
                                        opt, false,
#if LLVM_VERSION_MAJOR < 18
                                        true,
#endif
                                        CaptureDiagsKind::None, true, true, vfs);

    return {std::move(ast), modules};
  }

public:
  std::map<std::string, EntryType> units;
  explicit ClangContext(const std::vector<sv::ClangEntry> &entries, const std::string &baseDir)
      : context(),
        units(entries ^ map([&](auto &tu) { //
                return std::pair{tu.file, mkEntry(context, astBackingBuffer, baseDir, tu)};
              }) //
              ^ and_then([](auto &xs) { return std::map{xs.begin(), xs.end()}; })) {}
};

// === Tree ===
sv::Tree::Tree(const sv::SemanticTree<std::string> &root) : root(root) {}
size_t sv::Tree::nodes() const {
  return lazyNodes([&] {
    size_t n = 0;
    root.walk([&](auto &, auto) {
      n++;
      return true;
    });
    return n;
  });
}
size_t sv::Tree::maxDepth() const {
  return lazyMaxDepth([&] {
    size_t maxDepth = 0;
    root.walk([&](auto &, size_t depth) {
      maxDepth = std::max(maxDepth, depth);
      return true;
    });
    return maxDepth;
  });
}
size_t sv::Tree::maxWidth() const {
  return lazyMaxWidth([&] {
    std::vector<int> levelSize;
    root.walk([&](auto &, size_t depth) {
      if (depth >= levelSize.size()) { levelSize.resize(depth + 1); }
      levelSize[depth]++;
      return true;
    });
    return levelSize.empty() ? 0 : *std::max_element(levelSize.begin(), levelSize.end());
  });
}
sv::Tree sv::Tree::combine(const std::string &rootName, const std::vector<Tree> &trees) {
  return Tree{sv::SemanticTree<std::string>(rootName, trees ^ map([](auto &t) { return t.root; }))};
}
std::string sv::Tree::prettyPrint() const {
  std::stringstream ss;
  root.print(ss);
  return ss.str();
}
sv::Tree sv::Tree::leaf(const std::string &rootName) {
  return sv::Tree{sv::SemanticTree<std::string>(rootName, {})};
}

// === Source ===
sv::Source::Source(sv::TsTree tree) : tree(std::move(tree)) {}
const std::string &sv::Source::content() const { return tree.source; }
size_t sv::Source::sloc() const {
  return lazySloc([&] { return tree.sloc(); });
}
size_t sv::Source::lloc() const {
  return lazyLloc([&] { return tree.lloc(); });
}

std::set<uint32_t> sv::Source::slocLines() const {
  return lazySlocLines([&] { return tree.slocLines(); });
}
std::set<sv::Range> sv::Source::llocRanges() const {
  return lazyLlocRanges([&] {
    return tree.llocRanges() ^ map([](auto &start, auto &end) { return sv::Range{start, end}; });
  });
}
const sv::Tree &sv::Source::tsTree() const {
  return lazyTsTree([&] {
    return Tree{tree.template traverse<sv::SemanticTree<std::string>>(
        [](const auto &v) { return sv::SemanticTree{v, {}}; },
        [](auto &n, const auto &x) { n.children.emplace_back(x); })};
  });
}

// === Unit ===
sv::Unit::Unit(std::string path, sv::SemanticTree<std::string> sTree,
               sv::SemanticTree<std::string> sTreeInlined, sv::SemanticTree<std::string> irTree,
               TsTree source, TsTree preprocessedSource)
    : path_(std::move(path)), name_(std::filesystem::path(path_).filename()), //
      sTreeRoot(std::move(sTree)), sTreeInlinedRoot(std::move(sTreeInlined)),
      irTreeRoot(std::move(irTree)), sourceRoot(std::move(source)), //
      preprocessedRoot(std::move(preprocessedSource)) {}
const std::string &sv::Unit::path() const { return path_; }
const std::string &sv::Unit::name() const { return name_; }
const sv::Tree &sv::Unit::sTree() const { return sTreeRoot; }
const sv::Tree &sv::Unit::sTreeInlined() const { return sTreeInlinedRoot; }
const sv::Tree &sv::Unit::irTree() const { return irTreeRoot; }
sv::Source sv::Unit::writtenSource(bool normalise) const {
  return normalise ? lazyWrittenSourceNormalised([&]() {
    return Source(sourceRoot.deleteNodes("comment").normaliseNewLines().normaliseWhitespaces());
  })
                   : lazyWrittenSource([&]() { return Source(sourceRoot); });
}
sv::Source sv::Unit::preprocessedSource(bool normalise) const {
  return normalise ? lazyPreprocessedSourceNormalised([&]() {
    return Source(
        preprocessedRoot.deleteNodes("comment").normaliseNewLines().normaliseWhitespaces());
  })
                   : lazyPreprocessedSource([&]() { return Source(preprocessedRoot); });
}

// === Database ===

template <typename T> static std::vector<T> loadAll(const std::string &root) {
  std::vector<T> entries;
  try {
    for (auto &e : std::filesystem::directory_iterator(root)) {
      if (auto path = e.path(); path.string() ^ ends_with("sv.json")) {
        try {
          std::ifstream s(path);
          s.exceptions(std::ios::failbit | std::ios::badbit);
          T entry;
          nlohmann::from_json(nlohmann::json::parse(s), entry);
          entries.emplace_back(entry);
        } catch (const std::exception &e) { AGV_WARNF("Cannot load entry {}: {}", path, e); }
      }
    }
  } catch (const std::exception &e) { AGV_WARNF("Cannot list directory {}: {}", root, e); }
  return entries;
}

sv::Database sv::Codebase::loadDB(const std::string &root) {
  std::vector<std::variant<ClangEntry, FlatEntry>> entries;
  try {
    for (auto &e : std::filesystem::directory_iterator(root)) {
      if (auto path = e.path(); path.string() ^ ends_with("sv.json")) {
        try {
          std::ifstream s(path);
          s.exceptions(std::ios::failbit | std::ios::badbit);
          auto entry = nlohmann::json::parse(s);
          auto kind = entry.at("kind").get<std::string>();
          if (kind == "clang") {
            entries.emplace_back(entry.get<ClangEntry>());
          } else if (kind == "flat") {
            entries.emplace_back(entry.get<FlatEntry>());
          } else {
            AGV_WARNF("Unknown entry kind {} from {}", kind, path);
          }
        } catch (const std::exception &e) { AGV_WARNF("Cannot load entry {}: {}", path, e); }
      }
    }
  } catch (const std::exception &e) { AGV_WARNF("Cannot list directory {}: {}", root, e); }
  return {root, entries};
}

sv::Codebase sv::Codebase::load(const Database &db,                    //
                                std::ostream &out,                     //
                                bool normalise,                        //
                                const std::vector<std::string> &roots, //
                                const std::function<bool(const std::string &)> &predicate) {

  const auto select = [](auto x, auto f) { return std::visit([&](auto &&x) { return f(x); }, x); };

  const auto createTsParser = [](const std::string &language) -> TSLanguage * {
    if (language == "c") return tree_sitter_c();
    else if (language == "cpp") return tree_sitter_cpp();
    else if (language == "fortran") return tree_sitter_fortran();
    else if (language == "cuda") return tree_sitter_cuda();
    else if (language == "julia") return tree_sitter_julia();
    else if (language == "rust") return tree_sitter_rust();
    else {
      AGV_WARNF("Language {} is not supported by any included tree sitter parsers", language);
      return nullptr;
    }
  };

  const auto extractPreprocessedTsRoot = [&](const std::string &iiLines,
                                             const std::string &language) {
    const auto [witnessed, contents] = sv::parseCPPLineMarkers(iiLines);
    sv::TsTree tree{};
    if (!witnessed.empty()) {
      contents ^ get(witnessed.front()) ^
          for_each([&](auto &s) { tree = sv::TsTree(s, createTsParser(language)); });
    }
    return tree;
  };

  const auto selected =
      db.entries                                                                               //
      | filter([&](auto &x) { return select(x, [&](auto &x) { return predicate(x.file); }); }) //
      | to_vector();                                                                           //
  const auto maxFileLen =                                                                      //
      selected                                                                                 //
      | map([&](auto &x) {                                                                     //
          return select(x, [&](auto &x) { return static_cast<int>(x.file.size()); });          //
        })                                                                                     //
      | fold_left(int{}, [](auto l, auto r) { return std::max(l, r); });                       //

  // Load clang entries first
  const auto clangEntries = selected ^ collect([](auto &x) { return x ^ get<ClangEntry>(); });
  const auto clangUnits = sv::par_map(
      clangEntries,
      [&, clangCtx = ClangContext(clangEntries, db.root)](const auto &x) -> std::shared_ptr<Unit> {
        try {
          auto unitCtx = clangCtx.units ^ get(x.file);
          if (!unitCtx) {
            AGV_WARNF("Failed to load entry {}: cannot find context", x.file);
            return {};
          };
          auto &[ast, modules] = *unitCtx;
          sv::SemanticTree<std::string> irTreeRoot{"root", {}};
          for (auto &[name, module] : modules) {
            sv::SemanticTree<std::string> irTree{name, {}};
            sv::LLVMIRTreeVisitor(&irTree, *module, normalise);
            irTreeRoot.children.emplace_back(irTree);
          }

          sv::SemanticTree<std::string> sTree{"root", {}};
          sv::SemanticTree<std::string> sTreeInlined{"root", {}};
          sv::TsTree sourceRoot{};
          clang::SourceManager &sm = ast->getSourceManager();

          clang::LangOptions o = ast->getLangOpts();

          auto language = x.language;
          // we consider HIP/CUDA the same, but HIP also sets CUDA and not the other way around
          if (o.HIP || o.CUDA) language = "cuda";
          else if (o.CPlusPlus) language = "cpp";
          else if (o.C99 || o.C11 || o.C17 || o.C2x) language = "c";
          else
            AGV_WARNF("Cannot determine precise language from PCH for {}, using driver-based "
                      "language: {}",
                      x.file, language);

          if (auto data = sm.getBufferDataOrNone(sm.getMainFileID()); data)
            sourceRoot = sv::TsTree(data->str(), createTsParser(language));
          else
            AGV_WARNF("Failed to load original source for entry {}, main file ID missing", x.file);

          for (clang::Decl *decl :
               sv::topLevelDeclsInMainFile(*ast) ^ sort_by([&](clang::Decl *decl) {
                 return std::pair{sm.getDecomposedExpansionLoc(decl->getBeginLoc()).second,
                                  sm.getDecomposedExpansionLoc(decl->getEndLoc()).second};
               })) {

            auto createTree = [&](const sv::ClangASTSemanticTreeVisitor::Option &option) {
              sv::SemanticTree<std::string> topLevel{"toplevel", {}};
              sv::ClangASTSemanticTreeVisitor(&topLevel, ast->getASTContext(), option)
                  .TraverseDecl(decl);
              return topLevel;
            };
            sTree.children.emplace_back(createTree({
                .inlineCalls = false,
                .normaliseVarName = normalise, //
                .normaliseFnName = normalise,  //
                .roots = roots                 //
            }));
            sTreeInlined.children.emplace_back(createTree({
                .inlineCalls = true,
                .normaliseVarName = normalise, //
                .normaliseFnName = normalise,  //
                .roots = roots                 //
            }));
          }
          auto unit = std::make_shared<Unit>( //
              ast->getMainFileName().str(), sTree, sTreeInlined, irTreeRoot, sourceRoot,
              extractPreprocessedTsRoot(x.preprocessed, x.language));
          out << "# Loaded " << std::left << std::setw(maxFileLen) << x.file << "\r";
          return unit;
        } catch (const std::exception &e) {
          AGV_WARNF("Failed to load entry {}: {}", x.file, e);
          return {};
        }
      });

  // Then flat entries
  const auto flatEntries = selected ^ collect([](auto &x) { return x ^ get<FlatEntry>(); });
  auto flatUnits = sv::par_map(flatEntries, [&](const auto &x) -> std::shared_ptr<Unit> {
    try {
      auto loadTree = [](const std::filesystem::path &path) {
        sv::SemanticTree<std::string> tree;
        if (!std::filesystem::exists(path)) return tree;
        std::ifstream read(path);
        read.exceptions(std::ios::badbit | std::ios::failbit);
        nlohmann::from_json(nlohmann::json::parse(read), tree);
        return tree;
      };

      auto irtree = loadTree(
          fmt::format("{}/{}", db.root, normalise ? x.unnamedIRTreeFile : x.namedIRTreeFile));
      auto stree = loadTree(
          fmt::format("{}/{}", db.root, normalise ? x.unnamedSTreeFile : x.namedSTreeFile));

      auto source = x.dependencies                                                       //
                    | collect([&](auto &path, auto &dep) -> std::optional<std::string> { //
                        if (std::filesystem::path(path).filename() == x.file) return dep.content;
                        return std::nullopt;
                      }) //
                    | head_maybe();

      auto unit =
          std::make_unique<Unit>(x.file, stree, sv::SemanticTree<std::string>{}, irtree,
                                 sv::TsTree(source.value_or(""), createTsParser(x.language)),
                                 extractPreprocessedTsRoot(x.preprocessed, x.language));
      out << "# Loaded " << std::left << std::setw(maxFileLen) << x.file << "\r";
      return unit;
    } catch (const std::exception &e) {
      AGV_WARNF("Failed to load entry {}: {}", x.file, e);
      return {};
    }
  });
  out << std::endl;
  return sv::Codebase(db.root, clangUnits ^ concat(flatUnits));
}

namespace sv {
std::ostream &operator<<(std::ostream &os, const Range &range) {
  return os << "sv::Range{"                         //
            << ".start=" << range.startByte << ", " //
            << ".end=" << range.endByte             //
            << "}";
}

std::ostream &operator<<(std::ostream &os, const Codebase &codebase) {
  return os << "sv::Codebase{"                                        //
            << ".path=" << codebase.root                              //
            << ".units={" << (codebase.units ^ mk_string(",")) << "}" //
            << "}";
}
std::ostream &operator<<(std::ostream &os, const Unit &unit) {
  return os << "sv::Unit{"                                                     //
            << ".path=" << unit.path_ << ", "                                  //
            << ".name=" << unit.name_ << ", "                                  //
            << ".sTreeRoot=(" << unit.sTreeRoot.nodes() << "), "               //
            << ".sTreeInlinedRoot=(" << unit.sTreeInlinedRoot.nodes() << "), " //
            << ".irTreeRoot=(" << unit.irTreeRoot.nodes() << "), "
            << ".sourceRoot=" << unit.sourceRoot.root().tree //
            << "}";
}
} // namespace sv
