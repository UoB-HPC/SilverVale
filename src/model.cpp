#include <atomic>
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

using namespace sv;

class ClangContext {
  llvm::LLVMContext context;
  std::vector<std::vector<char>> astBackingBuffer;

  using EntryType = std::pair<std::shared_ptr<clang::ASTUnit>,
                              std::map<std::string, std::shared_ptr<llvm::Module>>>;

  static EntryType mkEntry(llvm::LLVMContext &llvmContext,          //
                           std::vector<std::vector<char>> &storage, //
                           const std::filesystem::path &baseDir,    //
                           const ClangEntry &tu) {
    auto modules =
        tu.bitcodes |
        collect([&](auto &entry)
                    -> std::optional<std::pair<std::string, std::shared_ptr<llvm::Module>>> {
          auto bcFile = baseDir / entry.file;
          auto buffer = llvm::MemoryBuffer::getFile(bcFile.string());
          if (auto ec = buffer.getError()) {
            SV_WARNF("Cannot load BC file  {}: ", bcFile, ec);
            return {};
          }

          auto module = llvm::parseBitcodeFile(buffer.get()->getMemBufferRef(), llvmContext);
          if (auto e = module.takeError()) {
            SV_WARNF("Cannot parse BC file {}: ", bcFile, toString(std::move(e)));
            return {};
          }

          return std::pair{entry.file, std::move(module.get())};
        }) |
        and_then([](auto &xs) { return std::map{xs.begin(), xs.end()}; });

    IntrusiveRefCntPtr<DiagnosticOptions> opts = new DiagnosticOptions();
    auto diagnostics = CompilerInstance::createDiagnostics(opts.get());

    IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> vfs = new llvm::vfs::InMemoryFileSystem();

    auto pchFile = baseDir / tu.pchFile;

    auto pchData = utils::zStdDecompress(pchFile);
    if (!pchData) {
      SV_WARNF("Cannot read PCH data: {}", pchFile);
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
  explicit ClangContext(const std::vector<std::shared_ptr<ClangEntry>> &entries,
                        const std::string &baseDir)
      : context(),
        units(entries ^ map([&](auto &tu) { //
                return std::pair{tu->file, mkEntry(context, astBackingBuffer, baseDir, *tu)};
              }) //
              ^ and_then([](auto &xs) { return std::map{xs.begin(), xs.end()}; })) {}
};

// === Tree ===
Tree::Tree(const NTree<SNode> &root) : root(root) {}
size_t Tree::nodes() const {
  return lazyNodes([&] {
    size_t n = 0;
    root.postOrderWalk([&](auto &, auto) { n++; });
    return n;
  });
}
size_t Tree::maxDepth() const {
  return lazyMaxDepth([&] {
    size_t maxDepth = 0;
    root.postOrderWalk([&](auto &, size_t depth) { maxDepth = std::max(maxDepth, depth); });
    return maxDepth;
  });
}
size_t Tree::maxWidth() const {
  return lazyMaxWidth([&] {
    std::vector<int> levelSize;
    root.postOrderWalk([&](auto &, size_t depth) {
      if (depth >= levelSize.size()) { levelSize.resize(depth + 1); }
      levelSize[depth]++;
    });
    return levelSize.empty() ? 0 : *std::max_element(levelSize.begin(), levelSize.end());
  });
}
Tree Tree::combine(const std::string &rootName, const std::vector<Tree> &trees) {
  return Tree{
      NTree<SNode>(SNode{rootName, Location{}}, trees ^ map([](auto &t) { return t.root; }))};
}
std::string Tree::prettyPrint() const {
  std::stringstream stream;
  root.print(stream);
  return stream.str();
}
Tree Tree::leaf(const std::string &rootName) {
  return Tree{NTree<SNode>(SNode{rootName, Location{}}, {})};
}

// === Source ===

Source::Source(TsTree tree, std::string content, const Mask &mask)
    : root_(std::move(tree)), content_(std::move(content)), mask(mask) {}
// XXX this content is different from that root_'s one as this can be modified for coverage
const std::string &Source::content() const { return content_; }

const std::string &Source::contentWhitespaceNormalised() const {
  return lazyNormalisedContent([&]() {
    // run a source-level (tree-unaware) WS normalisation here
    auto ws = [](char c) { return c == ' ' || c == '\t'; };
    std::string result;
    for (char c : content_) {
      if (result.empty()) {
        if (!ws(c) || c != '\n') result += c;
      } else {
        auto last = result.back();
        if (last == '\n' && c == '\n') continue; // delete consecutive NL
        if (ws(last) && ws(c)) continue;         // delete consecutive WS
        if (last == '\n' && ws(c)) continue;     // delete trailing WS after NL
        if (ws(last) && c == '\n') {
          result.pop_back();
          continue; // delete trailing WS before NL
        }
        result += c;
      }
    }
    return result;
  });
}
static std::pair<uint32_t, uint32_t> rowRange(TSNode n) {
  return {ts_node_start_point(n).row + 1,
          ts_node_end_point(n).row + 1}; // tree sitter row is 0-based
}

size_t Source::sloc() const {
  return lazySloc(
      [&] { return root_.slocLines([&](const TSNode n) { return mask(rowRange(n)); }).size(); });
}
size_t Source::lloc() const {
  return lazyLloc(
      [&] { return root_.llocRanges(([&](const TSNode n) { return mask(rowRange(n)); })).size(); });
}
std::set<uint32_t> Source::slocLines() const {
  return lazySlocLines([&] { return root_.slocLines(); });
}
std::set<Range> Source::llocRanges() const {
  return lazyLlocRanges([&] {
    return root_.llocRanges() ^ map([](auto &start, auto &end) { return Range{start, end}; });
  });
}
const Tree &Source::tsTree() const {
  return lazyTsTree([&] {
    auto root = root_.template traverse<NTree<TSNode>>(
        [&](const TSNode v) { return NTree{v, {}}; },
        [](auto &n, const auto &x) { n.children.emplace_back(x); });
    root.pruneInplace([&](const TSNode &n) { return mask(rowRange(n)); });
    return Tree(root.map<SNode>([&](const TSNode &n) {
      return SNode{std::string(ts_node_type(n)),
                   Location{.filename = root_.name,
                            .line = ts_node_start_point(n).row + 1, // tree sitter row is 0-based
                            .col = ts_node_start_point(n).column}};
    }));
  });
}

// === FlatCoverage ===

FlatCoverage::FlatCoverage(const PerFileCoverage &coverage) {
  for (auto &[file, instances] : coverage.filenames) {
    for (auto &instance : instances) {
      if (instance.count != 0) {
        for (size_t line = instance.lineStart; line <= instance.lineEnd; ++line) {
          entries[std::pair{file, line}] += instance.count;
        }
      }
    }
  }
}

// === Unit ===

Unit::Unit(std::string path, const std::shared_ptr<FlatCoverage> &coverage,    //
           NTree<SNode> sTree, NTree<SNode> sTreeInlined, NTree<SNode> irTree, //
           TsTree source, TsTree preprocessedSource)
    : path_(std::move(path)), name_(std::filesystem::path(path_).filename()),
      //      coverage(std::move(coverage)), //
      sTreeRoot(std::move(sTree)), sTreeInlinedRoot(std::move(sTreeInlined)),
      irTreeRoot(std::move(irTree)), sourceRoot(std::move(source)), //
      preprocessedRoot(std::move(preprocessedSource)), coverage(coverage) {}

const std::string &Unit::path() const { return path_; }
const std::string &Unit::name() const { return name_; }

NTree<SNode> Unit::pruneTree(const NTree<SNode> &tree) const {
  auto pruned = tree;
  pruned.pruneInplace([&](auto &s) {
    return coverage->entries.contains(std::pair{s.location.filename, s.location.line});
  });
  return pruned;
}

const Tree &Unit::sTree(View view) const {
  return lazySTree(view, [view, this]() {
    return view == View::WithCoverage //
               ? Tree(pruneTree(sTreeRoot.root))
               : sTreeRoot;
  });
}
const Tree &Unit::sTreeInlined(View view) const {
  return lazySTreeInlined(view, [view, this]() {
    return view == View::WithCoverage //
               ? Tree(pruneTree(sTreeInlinedRoot.root))
               : sTreeInlinedRoot;
  });
}
const Tree &Unit::irTree(View view) const {
  return lazyIrTree(view, [view, this]() {
    return view == View::WithCoverage ? Tree(pruneTree(irTreeRoot.root)) : irTreeRoot;
  });
}

TsTree Unit::normaliseTsTree(const TsTree &tree) {
  return tree.without("comment"); // // .normaliseWhitespaces();
}

const Source &Unit::sourceAsWritten() const {
  return lazySourceAsWritten([&]() {
    auto normalised = normaliseTsTree(sourceRoot);
    return Source(normalised, normalised.source, [](auto) { return true; });
  });
}
const Source &Unit::sourcePreprocessed() const {
  return lazySourcePreprocessed([&]() {
    auto normalised = normaliseTsTree(preprocessedRoot);
    return Source(normalised, normalised.source, [&](auto) { return true; });
  });
}
const Source &Unit::sourceWithCoverage() const {
  return lazySourceWithCoverage([&]() {
    // XXX use the original source but without comments to match up the lines
    auto normalised = normaliseTsTree(sourceRoot);
    auto coveragePrunedSource =
        (normalised.source ^ lines()) | zip_with_index(size_t{1})               //
        | filter([&](auto s, auto idx) {                                        //
            return coverage->entries.contains(std::pair{sourceRoot.name, idx}); //
          })                                                                    //
        | keys()                                                                //
        | filter([](auto x) { return !(x ^ is_blank()); })                      //
        | mk_string("\n");
    return Source(normalised, coveragePrunedSource, [&](auto range) {
      return inclusive(range.first, range.second) |
             exists([&](auto line) { return coverage->entries.contains(std::pair{name_, line}); });
    });
  });
}

// === Database ===

Database Codebase::loadDB(const std::string &root) {
  std::vector<std::variant<std::shared_ptr<ClangEntry>, std::shared_ptr<FlatEntry>>> entries;
  auto coverage = std::make_shared<PerFileCoverage>();
  auto parseJSON = [](auto path) {
    std::ifstream s(path);
    s.exceptions(std::ios::failbit | std::ios::badbit);
    return nlohmann::json::parse(s);
  };
  try {
    for (auto &entry : std::filesystem::directory_iterator(root)) {
      if (auto path = entry.path(); path.string() ^ ends_with(EntrySuffix)) {
        if (path.filename() == EntryClangSBCCName) {
          ClangSBCCProfile p;
          nlohmann::from_json(parseJSON(path), p);
          for (auto &e : p.data) {
            for (auto &f : e.functions) {
              for (auto &filename : f.filenames ^ distinct()) {
                auto name = std::filesystem::path(filename).filename();
                // XXX the first region encloses the entire function with a count of 1 iff it spans
                // more than one line
                if (f.regions.size() > 1 && f.regions[0].ExecutionCount == 1) {
                  // marker region: subtractive regions from this region onwards where 0 counts
                  // removes a region; we do this by first adding one instance per line then
                  // deleting it later; TODO this does not handle columns in any way
                  std::unordered_map<size_t, int64_t> counts;
                  for (size_t l = f.regions[0].LineStart; l <= f.regions[0].LineEnd; ++l)
                    counts.emplace(l, 1);
                  for (size_t i = 1; i < f.regions.size(); ++i) {
                    auto sub = f.regions[i];
                    int64_t delta =
                        sub.ExecutionCount > 0 ? int64_t(sub.ExecutionCount) : int64_t(-1);
                    for (size_t l = sub.LineStart; l <= sub.LineEnd; ++l)
                      counts[l] += delta;
                  }
                  for (auto &[line, count] : counts) {
                    coverage->filenames[name].emplace_back(PerFileCoverage::Instance{
                        .function = f.name,
                        .lineStart = line,
                        .lineEnd = line,
                        .colStart = 0,
                        .colEnd = 0,
                        .count = static_cast<size_t>(count < 0 ? 0 : count)});
                  }
                } else {
                  for (auto &r : f.regions) {
                    coverage->filenames[name].emplace_back(
                        PerFileCoverage::Instance{.function = f.name,
                                                  .lineStart = r.LineStart,
                                                  .lineEnd = r.LineEnd,
                                                  .colStart = r.ColumnStart,
                                                  .colEnd = r.ColumnEnd,
                                                  .count = r.ExecutionCount});
                  }
                }
              }
            }
          }
        } else if (path.filename() ^ ends_with(EntryGCCGCovName)) {
          // GCov is cumulative: it generates one profile per TU. We need to fold it into one.
          GCCGCovProfile p;
          nlohmann::from_json(parseJSON(path), p);
          for (auto &f : p.files) {
            for (auto &l : f.lines) {
              coverage->filenames[std::filesystem::path(f.file).filename()].emplace_back(
                  PerFileCoverage::Instance{.function = {},
                                            .lineStart = l.line_number,
                                            .lineEnd = l.line_number,
                                            .colStart = 0,
                                            .colEnd = 0,
                                            .count = l.count});
            }
          }
        } else {
          try {
            auto e = parseJSON(path);
            auto kind = e.at("kind").get<std::string>();
            if (kind == "clang") {
              auto p = std::make_shared<ClangEntry>();
              e.get_to(*p);
              entries.emplace_back(p);
            } else if (kind == "flat") {
              auto p = std::make_shared<FlatEntry>();
              e.get_to(*p);
              entries.emplace_back(p);
            } else {
              SV_WARNF("Unknown entry kind {} from {}", kind, path);
            }
          } catch (const std::exception &e) { SV_WARNF("Cannot load entry {}: {}", path, e); }
        }
      }
    }
  } catch (const std::exception &e) { SV_WARNF("Cannot list directory {}: {}", root, e); }

  SV_INFOF("Loaded DB {} with {} entries and {} coverage entries", root, entries.size(),
           coverage->filenames.size());

  return {root, entries, coverage};
}

Codebase Codebase::load(const Database &db,                    //
                        std::ostream &out,                     //
                        bool normalise,                        //
                        const std::vector<std::string> &roots, //
                        const std::function<bool(const std::string &)> &predicate) {

  const auto select = [](auto x, auto f) { return std::visit([&](auto &&x) { return f(*x); }, x); };

  const auto createTsParser = [](const std::string &language) -> TSLanguage * {
    if (language == "c") return tree_sitter_c();
    else if (language == "cpp") return tree_sitter_cpp();
    else if (language == "fortran") return tree_sitter_fortran();
    else if (language == "cuda") return tree_sitter_cuda();
    else if (language == "julia") return tree_sitter_julia();
    else if (language == "rust") return tree_sitter_rust();
    else {
      SV_WARNF("Language {} is not supported by any included tree sitter parsers", language);
      return nullptr;
    }
  };

  const auto extractPreprocessedTsRoot = [&](const std::string &name, const std::string &iiLines,
                                             const std::string &language) {
    const auto [witnessed, contents] = parseCPPLineMarkers(iiLines);
    TsTree tree{};
    if (!witnessed.empty()) {
      contents ^ get(witnessed.front()) ^
          for_each([&](auto &s) { tree = TsTree(name, s, createTsParser(language)); });
    }
    return tree;
  };

  auto findSourceInDeps = [](auto deps, auto file) {
    return deps                                                                 //
           | collect([&](auto &path, auto &dep) -> std::optional<std::string> { //
               if (std::filesystem::path(path).filename() == file) return dep.content;
               return std::nullopt;
             }) //
           | head_maybe();
  };

  auto flatCoverage =
      db.coverage ? std::make_shared<FlatCoverage>(*db.coverage) : std::make_shared<FlatCoverage>();

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
  const auto clangEntries =
      selected ^ collect([](auto &x) { return x ^ get<std::shared_ptr<ClangEntry>>(); });
  const auto clangUnits = par_map(
      clangEntries,
      [&, clangCtx = ClangContext(clangEntries, db.root)](const auto &x) -> std::shared_ptr<Unit> {
        try {
          auto unitCtx = clangCtx.units ^ get(x->file);
          if (!unitCtx) {
            SV_WARNF("Failed to load entry {}: cannot find context", x->file);
            return {};
          };
          auto &[ast, modules] = *unitCtx;
          NTree<SNode> irTreeRoot{{"root", Location{}}, {}};
          for (auto &[name, module] : modules) {
            NTree<SNode> irTree{{normalise ? "(bc)" : name, Location{}}, {}};
            LLVMIRTreeVisitor(&irTree, *module, normalise);
            irTreeRoot.children.emplace_back(irTree);
          }

          NTree<SNode> sTree{{"root", Location{}}, {}};
          NTree<SNode> sTreeInlined{{"root", Location{}}, {}};
          TsTree sourceRoot{};

          if (ast) {
            clang::SourceManager &sm = ast->getSourceManager();
            clang::LangOptions o = ast->getLangOpts();
            auto language = x->language;
            // we consider HIP/CUDA the same, but HIP also sets CUDA and not the other way around
            if (o.HIP || o.CUDA) language = "cuda";
            else if (o.CPlusPlus) language = "cpp";
            else if (o.C99 || o.C11 || o.C17 || o.C2x) language = "c";
            else
              SV_WARNF("Cannot determine precise language from PCH for {}, using driver-based "
                       "language: {}",
                        x->file, language);

            if (auto data = sm.getBufferDataOrNone(sm.getMainFileID()); data)
              sourceRoot = TsTree(x->file, data->str(), createTsParser(language));
            else
              SV_WARNF("Failed to load original source for entry {}, main file ID missing",
                       x->file);

            for (clang::Decl *decl :
                 topLevelDeclsInMainFile(*ast) ^ sort_by([&](clang::Decl *decl) {
                   return std::pair{sm.getDecomposedExpansionLoc(decl->getBeginLoc()).second,
                                    sm.getDecomposedExpansionLoc(decl->getEndLoc()).second};
                 })) {

              auto createTree = [&](const ClangASTSemanticTreeVisitor::Option &option) {
                NTree<SNode> topLevel{SNode{"toplevel", Location{}}, {}};
                ClangASTSemanticTreeVisitor(&topLevel, ast->getASTContext(), option)
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
          } else {
            SV_WARNF("Failed to load AST for entry {}", x->file);
            auto source = findSourceInDeps(x->dependencies, x->file);
            sourceRoot = TsTree(x->file, source.value_or(""), createTsParser(x->language));
          }

          auto name = ast ? ast->getMainFileName().str() : x->file;
          auto unit = std::make_shared<Unit>( //
              name, flatCoverage, sTree, sTreeInlined, irTreeRoot, sourceRoot,
              extractPreprocessedTsRoot(name, x->preprocessed, x->language));
          out << "# Loaded " << std::left << std::setw(maxFileLen) << x->file << "\r";
          return unit;
        } catch (const std::exception &e) {
          SV_WARNF("Failed to load entry {}: {}", x->file, e);
          return {};
        }
      });

  // Then flat entries
  const auto flatEntries =
      selected ^ collect([](auto &x) { return x ^ get<std::shared_ptr<FlatEntry>>(); });
  auto flatUnits = par_map(flatEntries, [&](const auto &x) -> std::shared_ptr<Unit> {
    try {
      auto loadTree = [](const std::filesystem::path &path) {
        NTree<SNode> tree;
        if (!std::filesystem::exists(path)) return tree;
        std::ifstream read(path);
        read.exceptions(std::ios::badbit | std::ios::failbit);
        nlohmann::from_json(nlohmann::json::parse(read), tree);
        return tree;
      };

      auto irtree = loadTree(
          fmt::format("{}/{}", db.root, normalise ? x->unnamedIRTreeFile : x->namedIRTreeFile));
      auto stree = loadTree(
          fmt::format("{}/{}", db.root, normalise ? x->unnamedSTreeFile : x->namedSTreeFile));

      auto source = findSourceInDeps(x->dependencies, x->file);
      auto unit =
          std::make_unique<Unit>(x->file, flatCoverage, stree, NTree<SNode>{}, irtree,
                                 TsTree(x->file, source.value_or(""), createTsParser(x->language)),
                                 extractPreprocessedTsRoot(x->file, x->preprocessed, x->language));
      out << "# Loaded " << std::left << std::setw(maxFileLen) << x->file << "\r";
      return unit;
    } catch (const std::exception &e) {
      SV_WARNF("Failed to load entry {}: {}", x->file, e);
      return {};
    }
  });
  out << std::endl;

  return Codebase(db.root, clangUnits ^ concat(flatUnits), flatCoverage);
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
