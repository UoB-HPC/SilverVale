#include <fstream>
#include <utility>

#include "agv/cli.h"
#include "agv/compress.h"
#include "agv/database.h"
#include "agv/par.h"

#include "clang/Frontend/ASTUnit.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "aspartame/map.hpp"
#include "aspartame/vector.hpp"
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
                           const agv::Database &db,                //
                           const std::string &baseDir,              //
                           const agv::Database::Entry &tu) {
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

    auto pchData = agv::utils::zStdDecompress(pchFile);
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
  explicit Context(const agv::Database &db, const std::string &baseDir)
      : context(), units(db.entries ^ map_values([&](auto &tu) {
                           return mkEntry(context, astBackingBuffer, db, baseDir, tu);
                         })) {}
};

// === Tree ===
agv::Tree::Tree(const agv::SemanticTree<std::string> &root) : root(root) {}
size_t agv::Tree::nodes() const {
  return lazyNodes([&] {
    size_t n = 0;
    root.walk([&](auto &, auto) {
      n++;
      return true;
    });
    return n;
  });
}
size_t agv::Tree::maxDepth() const {
  return lazyMaxDepth([&] {
    size_t maxDepth = 0;
    root.walk([&](auto &, size_t depth) {
      maxDepth = std::max(maxDepth, depth);
      return true;
    });
    return maxDepth;
  });
}
size_t agv::Tree::maxWidth() const {
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
agv::Tree agv::Tree::combine(const std::string &rootName, const std::vector<Tree> &trees) {
  return Tree{
      agv::SemanticTree<std::string>(rootName, trees ^ map([](auto &t) { return t.root; }))};
}
std::string agv::Tree::prettyPrint() const {
  std::stringstream ss;
  root.print([](auto &x) { return x; }, ss);
  return ss.str();
}
agv::Tree agv::Tree::leaf(const std::string &rootName) {
  return agv::Tree{agv::SemanticTree<std::string>(rootName, {})};
}

// === Source ===
agv::Source::Source(agv::TsTree tree) : tree(std::move(tree)) {}
const std::string &agv::Source::content() const { return tree.source; }
size_t agv::Source::sloc() const {
  return lazySloc([&] { return tree.sloc(); });
}
size_t agv::Source::lloc() const {
  return lazyLloc([&] { return tree.lloc(); });
}
const agv::Tree &agv::Source::tsTree() const {
  return lazyTsTree([&] {
    return Tree{tree.template traverse<agv::SemanticTree<std::string>>(
        [](const auto &v) { return agv::SemanticTree{v, {}}; },
        [](auto &n, const auto &x) { n.children.emplace_back(x); })};
  });
}

// === Unit ===
agv::Unit::Unit(std::string path, const agv::SemanticTree<std::string> &sTree,
                 const agv::SemanticTree<std::string> &sTreeInlined,
                 const agv::SemanticTree<std::string> &irTree, TsTree source)
    : path_(std::move(path)), name_(llvm::sys::path::filename(path_).str()), //
      sTreeRoot(sTree), sTreeInlinedRoot(sTreeInlined), irTreeRoot(irTree),
      sourceRoot(std::move(source)) {}
const std::string &agv::Unit::path() const { return path_; }
const std::string &agv::Unit::name() const { return name_; }
const agv::Tree &agv::Unit::sTree() const { return sTreeRoot; }
const agv::Tree &agv::Unit::sTreeInlined() const { return sTreeInlinedRoot; }
const agv::Tree &agv::Unit::irTree() const { return irTreeRoot; }
agv::Source agv::Unit::source(bool normalise) const {
  return normalise
             ? lazySourceNormalised([&]() { return Source(sourceRoot.deleteNodes("comment")); })
             : lazySource([&]() { return Source(sourceRoot); });
}

// === Database ===
agv::Database agv::Database::fromJsonString(const std::string &json) {
  agv::Database database;
  nlohmann::json dbJson = nlohmann::json::parse(json);
  nlohmann::from_json(dbJson, database);
  return database;
}
agv::Database agv::Database::fromJsonStream(std::ifstream &stream) {
  agv::Database database;
  nlohmann::json dbJson = nlohmann::json::parse(stream);
  nlohmann::from_json(dbJson, database);
  return database;
}
agv::Database agv::Database::fromJsonFile(const std::string &file) {
  std::ifstream s(file);
  s.exceptions(std::ios::failbit | std::ios::badbit);
  return agv::Database::fromJsonStream(s);
}
agv::Codebase agv::Database::load(std::ostream &out,                     //
                     bool normalise,                        //
                     const std::string &path,               //
                     const std::vector<std::string> &roots, //
                     const std::function<bool(const std::string &)> &predicate) const {

  Context ctx(*this, path);

  auto selected = ctx.units | keys() | filter(predicate) | to_vector();

  auto maxFileLen =
      static_cast<int>(selected | map([](auto &k) { return k.size(); }) |
                       fold_left(size_t{}, [](auto l, auto r) { return std::max(l, r); }));

  std::vector<std::shared_ptr<Unit>> units(selected.size());
  agv::par_for(selected, [&](auto &key, auto idx) {
    auto &[ast, modules] = ctx.units[key];

    agv::SemanticTree<std::string> irTreeRoot{"root", {}};
    for (auto &[name, module] : modules) {
      agv::SemanticTree<std::string> irTree{name, {}};
      agv::LLVMIRTreeVisitor(&irTree, *module, true);
      irTreeRoot.children.emplace_back(irTree);
    }

    agv::SemanticTree<std::string> sTree{"root", {}};
    agv::SemanticTree<std::string> sTreeInlined{"root", {}};
    auto &sm = ast->getSourceManager();
    if (auto data = sm.getBufferDataOrNone(sm.getMainFileID()); data) {
      for (clang::Decl *decl :
           agv::topLevelDeclsInMainFile(*ast) ^ sort_by([&](clang::Decl *decl) {
             return std::pair{sm.getDecomposedExpansionLoc(decl->getBeginLoc()).second,
                              sm.getDecomposedExpansionLoc(decl->getEndLoc()).second};
           })) {

        auto createTree = [&](const agv::ClangASTSemanticTreeVisitor::Option &option) {
          agv::SemanticTree<std::string> topLevel{"toplevel", {}};
          agv::ClangASTSemanticTreeVisitor(&topLevel, ast->getASTContext(), option)
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

      auto tsTree = agv::TsTree(data->str(), tree_sitter_cpp()).deleteNodes("comment");
      units[idx] = std::make_unique<Unit>(ast->getMainFileName().str(), sTree, sTreeInlined,
                                          irTreeRoot, tsTree);
    } else {
      std::cerr << "Failed to load AST for " << key << ", stree data will be empty!" << std::endl;
    }
    out << "# Loaded " << std::left << std::setw(maxFileLen) << key << "\r";
  });
  out << std::endl;
  return agv::Codebase(path, units);
}

namespace agv {
std::ostream &operator<<(std::ostream &os, const Database::Entry &entry) {
  return os << "agv::Database::Entry{"                                                          //
            << ".pchName=" << entry.pchName << ", "                                              //
            << ".compileCommands=" << (entry.compileCommands | mk_string("{", ",", "}")) << ", " //
            << ".dependencies=(" << entry.dependencies.size() << ")" << ", "                     //
            << ".bitcodes=" << (entry.bitcodes | mk_string("{", ",", "}"))                       //
            << "}";
}
std::ostream &operator<<(std::ostream &os, const Database::Bitcode &bitcode) {
  return os << "agv::Database::Bitcode{"       //
            << ".name=" << bitcode.name << ", " //
            << ".kind=" << bitcode.kind << ", " //
            << ".triple=" << bitcode.triple     //
            << "}";
}
std::ostream &operator<<(std::ostream &os, const Database &database) {
  os << "agv::Database{"; //
  for (auto &[k, v] : database.attributes)
    os << ".`" << k << "`=" << v << ", ";
  os << ".entries=" << (database.entries | values() | mk_string("{", ",", "}")) << ", " //
     << ".dependencies=(" << database.dependencies.size() << ")"                        //
     << "}";
  return os;
}

std::ostream &operator<<(std::ostream &os, const Codebase &codebase) {
  return os << "agv::Codebase{"                                      //
            << ".path=" << codebase.path                              //
            << ".units={" << (codebase.units ^ mk_string(",")) << "}" //
            << "}";
}
std::ostream &operator<<(std::ostream &os, const Unit &unit) {
  return os << "agv::Unit{"                                                   //
            << ".path=" << unit.path_ << ", "                                  //
            << ".name=" << unit.name_ << ", "                                  //
            << ".sTreeRoot=(" << unit.sTreeRoot.nodes() << "), "               //
            << ".sTreeInlinedRoot=(" << unit.sTreeInlinedRoot.nodes() << "), " //
            << ".irTreeRoot=(" << unit.irTreeRoot.nodes() << "), "
            << ".sourceRoot=" << unit.sourceRoot.root().tree //
            << "}";
}
} // namespace agv
