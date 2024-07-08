#include <iosfwd>
#include <utility>

#include "p3md/compress.h"
#include "p3md/database.h"
#include "p3md/par.h"

#include "clang/Frontend/ASTUnit.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "aspartame/map.hpp"
#include "aspartame/view.hpp"
#include "tree_sitter_cpp/api.h"

using namespace aspartame;
using namespace clang;

class Context {
  llvm::LLVMContext context;
  std::vector<std::vector<char>> astBackingBuffer;

  using EntryType = std::pair<std::unique_ptr<clang::ASTUnit>,
                              std::map<std::string, std::shared_ptr<llvm::Module>>>;

  static EntryType mkEntry(llvm::LLVMContext &llvmContext,          //
                           std::vector<std::vector<char>> &storage, //
                           const p3md::Database &db,                //
                           const std::string &baseDir,              //
                           const p3md::Database::Entry &tu) {
    auto modules =
        tu.bitcodes |
        collect([&](auto &entry)
                    -> std::optional<std::pair<std::string, std::shared_ptr<llvm::Module>>> {
          auto bcFile = baseDir + "/" + entry.name;

          auto buffer = llvm::MemoryBuffer::getFile(bcFile);
          if (auto ec = buffer.getError()) {
            std::cerr << "Cannot load BC file " << bcFile << ": " << ec.message() << std::endl;
            return {};
          }

          auto module = llvm::parseBitcodeFile(buffer.get()->getMemBufferRef(), llvmContext);
          if (auto e = module.takeError()) {
            std::cerr << "Cannot parse BC file" << bcFile << toString(std::move(e)) << std::endl;
            return {};
          }

          return std::pair{entry.name, std::move(module.get())};
        }) |
        and_then([](auto &xs) { return std::map{xs.begin(), xs.end()}; });

    IntrusiveRefCntPtr<DiagnosticOptions> opts = new DiagnosticOptions();
    auto diagnostics = CompilerInstance::createDiagnostics(opts.get());

    IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> vfs = new llvm::vfs::InMemoryFileSystem();

    auto pchFile = baseDir + "/" + tu.pchName;

    auto pchData = p3md::utils::zStdDecompress(pchFile);
    if (!pchData) {
      std::cerr << "Cannot read PCH data:" << pchFile << std::endl;
      return {nullptr, modules};
    }
    auto &backing = storage.emplace_back(*pchData);

    auto mb = llvm::MemoryBuffer::getMemBuffer(llvm::StringRef(backing.data(), backing.size()), "",
                                               false);
    vfs->addFile(pchFile, 0, std::move(mb));
    for (auto &[name, actual] : tu.dependencies) {
      if (auto source = db.dependencies.find(actual); source != db.dependencies.end()) {
        vfs->addFile(name, source->second.modified,
                     llvm::MemoryBuffer::getMemBuffer(source->second.content));
      } else {
        std::cerr << "Cannot find dependency " << actual << " in db!" << std::endl;
      }
    }

    auto opt = std::make_shared<clang::HeaderSearchOptions>();
    auto ast = ASTUnit::LoadFromASTFile(pchFile,                          //
                                        clang::RawPCHContainerReader(),   //
                                        ASTUnit::WhatToLoad::LoadASTOnly, //
                                        diagnostics,                      //
                                        clang::FileSystemOptions(""),     //
                                        opt, false, true, CaptureDiagsKind::None, true, true, vfs);

    return {std::move(ast), modules};
  }

public:
  std::map<std::string, EntryType> units;
  explicit Context(const p3md::Database &db, const std::string &baseDir)
      : context(), units(db.entries ^ map_values([&](auto &tu) {
                           return mkEntry(context, astBackingBuffer, db, baseDir, tu);
                         })) {}
};

// === Tree ===
p3md::Tree::Tree(const p3md::SemanticTree<std::string> &root) : root(root) {}
size_t p3md::Tree::nodes() const {
  return lazyNodes([&] {
    size_t n = 0;
    root.walk([&](auto &, auto) {
      n++;
      return true;
    });
    return n;
  });
}
size_t p3md::Tree::maxDepth() const {
  return lazyMaxDepth([&] {
    size_t maxDepth = 0;
    root.walk([&](auto &, size_t depth) {
      maxDepth = std::max(maxDepth, depth);
      return true;
    });
    return maxDepth;
  });
}
size_t p3md::Tree::maxWidth() const {
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
p3md::Tree p3md::Tree::combine(const std::string &rootName, const std::vector<Tree> &trees) {
  return Tree{
      p3md::SemanticTree<std::string>(rootName, trees ^ map([](auto &t) { return t.root; }))};
}
std::string p3md::Tree::prettyPrint() const {
  std::stringstream ss;
  root.print([](auto &x) { return x; }, ss);
  return ss.str();
}
p3md::Tree p3md::Tree::leaf(const std::string &rootName) {
  return p3md::Tree{p3md::SemanticTree<std::string>(rootName, {})};
}

// === Source ===
p3md::Source::Source(p3md::TsTree tree) : tree(std::move(tree)) {}
const std::string &p3md::Source::content() const { return tree.source; }
size_t p3md::Source::sloc() const {
  return lazySloc([&] { return tree.sloc(); });
}
size_t p3md::Source::lloc() const {
  return lazyLloc([&] { return tree.lloc(); });
}
const p3md::Tree &p3md::Source::tsTree() const {
  return lazyTsTree([&] {
    return Tree{tree.template traverse<p3md::SemanticTree<std::string>>(
        [](const auto &v) { return p3md::SemanticTree{v, {}}; },
        [](auto &n, const auto &x) { n.children.emplace_back(x); })};
  });
}

// === Unit ===
p3md::Unit::Unit(std::string path, const p3md::SemanticTree<std::string> &sTree,
                 const p3md::SemanticTree<std::string> &sTreeInlined,
                 const p3md::SemanticTree<std::string> &irTree, TsTree source)
    : path_(std::move(path)), name_(llvm::sys::path::filename(path_).str()), //
      sTreeRoot(sTree), sTreeInlinedRoot(sTreeInlined), irTreeRoot(irTree),
      sourceRoot(std::move(source)) {}
const std::string &p3md::Unit::path() const { return path_; }
const std::string &p3md::Unit::name() const { return name_; }
const p3md::Tree &p3md::Unit::sTree() const { return sTreeRoot; }
const p3md::Tree &p3md::Unit::sTreeInlined() const { return sTreeInlinedRoot; }
const p3md::Tree &p3md::Unit::irTree() const { return irTreeRoot; }
p3md::Source p3md::Unit::source(bool normalise) const {
  return normalise
             ? lazySourceNormalised([&]() { return Source(sourceRoot.deleteNodes("comment")); })
             : lazySource([&]() { return Source(sourceRoot); });
}

// === Database ===
p3md::Database p3md::Database::fromJson(const std::string &json) {
  p3md::Database database;
  nlohmann::json dbJson = nlohmann::json::parse(json);
  nlohmann::from_json(dbJson, database);
  return database;
}
p3md::Database p3md::Database::fromJsonFile(const std::string &file) {
  auto buffer = llvm::MemoryBuffer::getFile(file, /*IsText*/ true);
  if (auto e = buffer.getError()) {
    throw std::logic_error(e.message());
  } else return p3md::Database::fromJson((*buffer)->getBuffer().str());
}
p3md::Codebase p3md::Database::load(std::ostream &out,                     //
                                    bool normalise,                        //
                                    const std::string &path,               //
                                    const std::vector<std::string> &roots, //
                                    const std::function<bool(std::string)> &predicate) const {

  Context ctx(*this, path);

  auto selected = ctx.units | keys() | filter(predicate) | to_vector();

  auto maxFileLen =
      static_cast<int>(selected | map([](auto &k) { return k.size(); }) |
                       fold_left(size_t{}, [](auto l, auto r) { return std::max(l, r); }));

  std::vector<std::unique_ptr<Unit>> units(selected.size());
  p3md::par_for(selected, [&](auto &key, auto idx) {
    auto &[ast, modules] = ctx.units[key];

    p3md::SemanticTree<std::string> irTreeRoot{"root", {}};
    for (auto &[name, module] : modules) {
      p3md::SemanticTree<std::string> irTree{name, {}};
      p3md::LLVMIRTreeVisitor(&irTree, *module, true);
      irTreeRoot.children.emplace_back(irTree);
    }

    p3md::SemanticTree<std::string> sTree{"root", {}};
    p3md::SemanticTree<std::string> sTreeInlined{"root", {}};
    auto &sm = ast->getSourceManager();
    if (auto data = sm.getBufferDataOrNone(sm.getMainFileID()); data) {
      for (clang::Decl *decl :
           p3md::topLevelDeclsInMainFile(*ast) ^ sort_by([&](clang::Decl *decl) {
             return std::pair{sm.getDecomposedExpansionLoc(decl->getBeginLoc()).second,
                              sm.getDecomposedExpansionLoc(decl->getEndLoc()).second};
           })) {

        auto createTree = [&](const p3md::ClangASTSemanticTreeVisitor::Option &option) {
          p3md::SemanticTree<std::string> topLevel{"toplevel", {}};
          p3md::ClangASTSemanticTreeVisitor(&topLevel, ast->getASTContext(), option)
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

      auto tsTree = p3md::TsTree(data->str(), tree_sitter_cpp()).deleteNodes("comment");
      units[idx] = std::make_unique<Unit>(ast->getMainFileName().str(), sTree, sTreeInlined,
                                          irTreeRoot, tsTree);
    } else {
      std::cout << "Failed to load AST for " << key << ", stree data will be empty!" << std::endl;
    }
    out << "# Loaded " << std::left << std::setw(maxFileLen) << key << "\r";
  });
  out << std::endl;
  return p3md::Codebase(path, units ^ map([](auto &x) { return *std::move(x); }));
}

namespace p3md {
std::ostream &operator<<(std::ostream &os, const p3md::Database::Entry &entry) {
  return os << "p3md::Database::Entry{"                                                          //
            << ".pchName=" << entry.pchName << ", "                                              //
            << ".compileCommands=" << (entry.compileCommands | mk_string("{", ",", "}")) << ", " //
            << ".dependencies=(" << entry.dependencies.size() << ")" << ", "                     //
            << ".bitcodes=" << (entry.bitcodes | mk_string("{", ",", "}"))                       //
            << "}";
}
std::ostream &operator<<(std::ostream &os, const p3md::Database::Bitcode &bitcode) {
  return os << "p3md::Database::Bitcode{"       //
            << ".name=" << bitcode.name << ", " //
            << ".kind=" << bitcode.kind << ", " //
            << ".triple=" << bitcode.triple     //
            << "}";
}
std::ostream &operator<<(std::ostream &os, const p3md::Database &database) {
  return os << "p3md::Database{"                                                               //
            << ".clangMajorVersion=" << database.clangMajorVersion << ", "                     //
            << ".clangMinorVersion=" << database.clangMinorVersion << ", "                     //
            << ".clangPatchVersion=" << database.clangPatchVersion << ", "                     //
            << ".entries=" << (database.entries | values() | mk_string("{", ",", "}")) << ", " //
            << ".dependencies=(" << database.dependencies.size() << ")"                        //
            << "}";
}

} // namespace p3md
