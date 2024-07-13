#include <iostream>

#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/ASTUnit.h"

#include "aspartame/optional.hpp"
#include "aspartame/set.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "agv/tree.h"

using namespace clang;
using namespace aspartame;

std::vector<Decl *> agv::topLevelDeclsInMainFile(ASTUnit &unit) {
  std::vector<Decl *> xs;
  unit.visitLocalTopLevelDecls(&xs, [](auto xsPtr, auto decl) {
    reinterpret_cast<decltype(xs) *>(xsPtr)->push_back(const_cast<Decl *>(decl));
    return true;
  });
  return xs ^
         filter([&](Decl *d) { return unit.getSourceManager().isInMainFile(d->getLocation()); });
}

static inline CXXMethodDecl *extractLambdaCallMethodFromDeclRefTpe(Expr *expr) {
  if (auto ref = llvm::dyn_cast<DeclRefExpr>(expr->IgnoreUnlessSpelledInSource()); ref) {
    if (auto recordTpe = llvm::dyn_cast<RecordType>(ref->getType().getCanonicalType()); recordTpe) {

      if (auto cxxRecordDecl = llvm::dyn_cast<CXXRecordDecl>(recordTpe->getDecl());
          cxxRecordDecl && cxxRecordDecl->isLambda()) {

        return cxxRecordDecl->getLambdaCallOperator();
      }
    }
  }
  return {};
}

agv::ClangASTSemanticTreeVisitor::ClangASTSemanticTreeVisitor(
    agv::SemanticTree<std::string> *root, clang::ASTContext &Context,
    agv::ClangASTSemanticTreeVisitor::Option option)
    : SemanticTreeVisitor(root), Context(Context), option(std::move(option)) {}

bool agv::ClangASTSemanticTreeVisitor::TraverseStmt(clang::Stmt *stmt) { // NOLINT(*-no-recursion)
  // Remove implicits
  if (Expr *expr = llvm::dyn_cast_or_null<Expr>(stmt); expr) {
    stmt = expr->IgnoreUnlessSpelledInSource();
  }
  if (!stmt) return single("<<<NULL>>>");

  return visitDyn<bool>(
             stmt,
             [&](CompoundStmt *compound) {
               return compound->body() | forall([&](auto s) { return TraverseStmt(s); });
             },
             [&](DeclStmt *decl) {
               return decl->decls() | forall([&](auto s) { return TraverseDecl(s); });
             },
             [&](IntegerLiteral *lit) {
               return single(std::string(stmt->getStmtClassName()) + ": " +
                             std::to_string(lit->getValue().getLimitedValue()));
             },
             [&](FloatingLiteral *lit) {
               return single(std::string(stmt->getStmtClassName()) + ": " +
                             std::to_string(lit->getValue().convertToDouble()));
             },
             [&](StringLiteral *lit) {
               return single(std::string(stmt->getStmtClassName()) + ": \"" +
                             (lit->getString().str() ^ replace_all("\n", "\\n")) + "\"");
             }) ^
         fold([&]() {
           auto name = std::string(stmt->getStmtClassName());
           auto &sm = Context.getSourceManager();

           // Try to inline calls
           auto handleFn = [&](CallExpr *call, FunctionDecl *direct) {
             name += +": " + direct->getDeclName().getAsString();

             bool projectSymbol = option.roots | exists([&](auto root) {
                                    return sm.getFilename(direct->getLocation()).starts_with(root);
                                  });
             if (projectSymbol && direct->hasBody()) { // Symbol part of project root, inline

               //               node->children.emplace_back("Inline: " +
               //               direct->getDeclName().getAsString() +
               //                                           " @ " +
               //                                           sm.getFilename(direct->getLocation()).str());

               return TraverseStmt(direct->getBody());

             } else { // Symbol outside the project root, expand all executables here instead
                      //               node->children.emplace_back("No Inline: " +
                      //               direct->getDeclName().getAsString() +
                      //                                           " @ " +
               //                                           sm.getFilename(direct->getLocation()).str());
               return scoped(
                   [&]() {
                     return call->arguments() | forall([&](Expr *arg) {
                              // for DeclRefs to a lambda in a no-inline case, expand the body here
                              if (auto lambdaApply = extractLambdaCallMethodFromDeclRefTpe(arg);
                                  lambdaApply) {
                                node->children.emplace_back("App: " +
                                                            lambdaApply->getNameAsString());
                                return TraverseStmt(lambdaApply->getBody());
                              } else {
                                return TraverseStmt(arg);
                              }
                            });
                   },
                   name);
             }
           };

           if (auto *call = llvm::dyn_cast_or_null<CallExpr>(stmt); option.inlineCalls && call) {
             // Call to a real function decl in the project, try to inline
             if (auto *overload = llvm::dyn_cast_or_null<OverloadExpr>(call->getCallee());
                 overload) {
               auto direct = overload->decls() |
                             collect([](NamedDecl *x) -> std::optional<FunctionDecl *> {
                               if (auto fn = llvm::dyn_cast_or_null<FunctionDecl>(x); fn) return fn;
                               return {};
                             }) |
                             head_maybe();
               if (direct) { return handleFn(call, *direct); }
             } else if (auto direct = call->getDirectCallee(); direct) {
               return handleFn(call, direct);
             } else {

               // Call to non-functions:
               //  lambda apply, unapplied template fns, unresolved ADL calls
             }
           }

           auto suffix = visitDyn<std::string>(
               stmt, //
               [&](BinaryOperator *bin) { return bin->getOpcodeStr().str(); },
               [&](DeclRefExpr *ref) {
                 return option.normaliseVarName ? "" : ref->getDecl()->getDeclName().getAsString();
               });

           return scoped(
               [&]() {
                 return RecursiveASTVisitor<ClangASTSemanticTreeVisitor>::TraverseStmt(stmt);
               },
               suffix ^ map([&](auto s) { return name + ": " + s; }) ^ get_or_else(name));
         });
}

bool agv::ClangASTSemanticTreeVisitor::TraverseDecl(clang::Decl *decl) { // NOLINT(*-no-recursion)
  if (!decl) return true;
  auto suffix = visitDyn<std::string>(
      decl, //
      [&](FunctionDecl *fn) {
        return option.normaliseFnName ? "" : fn->getDeclName().getAsString();
      },
      [&](VarDecl *var) {
        return option.normaliseVarName ? "" : var->getDeclName().getAsString();
      });
  std::string name = decl->getDeclKindName();
  return scoped(
      [&]() { return RecursiveASTVisitor<ClangASTSemanticTreeVisitor>::TraverseDecl(decl); },
      suffix ^ map([&](auto s) { return name + ": " + s; }) ^ get_or_else(name));
}

agv::TsTree::TsTree() = default;
agv::TsTree::TsTree(const std::string &source, const TSLanguage *lang)
    : source(source),
      parser(std::shared_ptr<TSParser>(ts_parser_new(), [](auto x) { ts_parser_delete(x); })) {
  ts_parser_set_language(parser.get(), lang);
  tree = std::shared_ptr<TSTree>(
      ts_parser_parse_string(parser.get(), nullptr, source.c_str(), source.size()),
      [](auto x) { ts_tree_delete(x); });
}

TSNode agv::TsTree::root() const { return ts_tree_root_node(tree.get()); }

static void deleteNodes(const TSNode &node, const std::string &type, size_t &offset,
                        std::string &out);
static void deleteNodes( // NOLINT(*-no-recursion)
    const TSNode &node, const std::string &type, size_t &offset, std::string &out) {
  if (std::string(ts_node_type(node)) == type) {
    auto start = ts_node_start_byte(node);
    auto end = ts_node_end_byte(node);
    out.erase(start - offset, end - start);
    offset += end - start;
  } else {
    for (uint32_t i = 0; i < ts_node_child_count(node); ++i) {
      deleteNodes(ts_node_child(node, i), type, offset, out);
    }
  }
}

agv::TsTree agv::TsTree::deleteNodes(const std::string &type,
                                     const std::optional<TSNode> &node) const {
  size_t offset = 0;
  std::string out = source;
  ::deleteNodes(node.value_or(root()), type, offset, out);
  return {out, ts_parser_language(parser.get())};
}


agv::TsTree agv::TsTree::normaliseNewLines(const std::optional<TSNode> &node) const {
  auto linesToKeep = slocLines(node.value_or(root()));
  auto slocNormalise =
      (source ^ lines())                                                    //
      | zip_with_index<uint32_t>()                                          //
      | filter([&](auto l, auto idx) { return linesToKeep.contains(idx); }) //
      | fold_left(std::string{}, [](auto acc, auto x) { return acc + "\n" + x.first; });
  return {slocNormalise ^ trim(), ts_parser_language(parser.get())};
}

void markWSRanges( // NOLINT(*-no-recursion)
    const TSNode &root, const std::string &content, uint32_t maxWs,
    std::vector<std::pair<uint32_t, uint32_t>> &xs) {
  auto count = ts_node_child_count(root);
  for (uint32_t i = 0; i < count; ++i) {
    auto node = ts_node_child(root, i);
    if (i + 1 < count) {
      auto next = ts_node_child(root, i + 1);
      auto start = ts_node_end_byte(node), end = ts_node_start_byte(next);
      if (end - start > maxWs) {
        if (auto nlIdx = content.substr(start, end - start) ^ index_of('\n'); nlIdx != -1) {
          // delete anything before and after N*NL, but not the NL itself
          // Example with @=' ': `int a;@@\n@@@//` becomes `int a'@\n//`
          xs.emplace_back(start, start + nlIdx);
          xs.emplace_back(start + nlIdx + maxWs, end);
        } else {
          xs.emplace_back(start + maxWs, end); // keep N*WS
        }
      }
    }
    markWSRanges(node, content, maxWs, xs);
  }
}

agv::TsTree agv::TsTree::normaliseWhitespaces(uint32_t maxWS,
                                              const std::optional<TSNode> &node) const {
  std::string out = source;
  std::vector<std::pair<uint32_t, uint32_t>> ranges;
  markWSRanges(node.value_or(root()), out, maxWS, ranges);
  if (ranges.empty()) { return {out, ts_parser_language(parser.get())}; }

  auto sortedRanges = ranges ^ sort();
  std::vector<std::pair<uint32_t, uint32_t>> merged{sortedRanges[0]};
  for (auto r : sortedRanges) {
    auto &last = merged.back();
    if (r.first <= last.second) last.second = std::max(last.second, r.second);
    else merged.push_back(r);
  }
  for (auto it = sortedRanges.rbegin(); it != sortedRanges.rend(); ++it) {
    out.erase(it->first, it->second - it->first);
  }
  return {out, ts_parser_language(parser.get())};
}

std::set<uint32_t> agv::TsTree::slocLines(const std::optional<TSNode> &node) const {
  std::set<uint32_t> slocLines;
  walk(
      [&](TSNode x) {
        std::string type = ts_node_type(x);

        if (type == "translation_unit" || type == "program") {
          // Some parser's TU would use the end of the file as end point
          // Fortran's TU conditionally contain an extra top-level `program` container if used
          return true;
        }

        if (type.starts_with("preproc_")) {
          // preproc adds a trailing newline, so we just parse what's actually inside
          return true;
        }

        if (type == "end_program_statement" && ts_node_named_child_count(x) == 1) {
          // XXX handle Fortran's trailing newline after `end program $name`
          slocLines.emplace(ts_node_start_point(x).row);
          slocLines.emplace(ts_node_end_point(ts_node_named_child(x, 0)).row);
          return true;
        }

        slocLines.emplace(ts_node_start_point(x).row);
        slocLines.emplace(ts_node_end_point(x).row);
        return true;
      },
      node);
  return slocLines;
}

std::set<std::pair<uint32_t, uint32_t>>
agv::TsTree::llocRanges(const std::optional<TSNode> &node) const {
  std::set<std::pair<uint32_t, uint32_t>> llocRanges;
  walk(
      [&](auto x) {
        std::string type = ts_node_type(x);
        // don't count compound stmt as it contains children
        if (type == "compound_statement") return true;

        if (type == "end_program_statement" && ts_node_named_child_count(x) == 1) {
          // XXX handle Fortran's trailing newline after `end program $name`
          llocRanges.emplace(ts_node_start_byte(x), ts_node_end_byte(ts_node_named_child(x, 0)));
          return true;
        }

        if (type ^ ends_with("declaration") || //
            type ^ ends_with("statement") ||   //
            type ^ ends_with("directive")      //
        ) {
          llocRanges.emplace(ts_node_start_byte(x), ts_node_end_byte(x));
        }
        return true;
      },
      node);
  return llocRanges;
}

size_t agv::TsTree::sloc(const std::optional<TSNode> &node) const { //
  return slocLines(node).size();
}
size_t agv::TsTree::lloc(const std::optional<TSNode> &node) const { //
  return llocRanges(node).size();
}

static std::string print(llvm::Type *t) {
  std::string s;
  llvm::raw_string_ostream rso(s);
  t->print(rso);
  return rso.str();
}

agv::LLVMIRTreeVisitor::LLVMIRTreeVisitor(agv::SemanticTree<std::string> *root,
                                          const llvm::Module &module, bool normaliseName)
    : SemanticTreeVisitor(root), normaliseName(normaliseName) {

  module.globals() | for_each([&](const llvm::GlobalVariable &var) {
    auto name = named("Global", var.getName().str() + ": " + print(var.getType()));
    if (var.hasInitializer()) {
      scoped([&]() { walk(var.getInitializer()); }, name);
    } else {
      single(name);
    }
  });
  module.functions() | for_each([&](const llvm::Function &fn) {
    auto args =
        fn.args() | mk_string(",", [&](const llvm::Argument &arg) { return print(arg.getType()); });
    auto sig =
        ((!normaliseName ? fn.getName() : "") + "(" + args + "): " + print(fn.getReturnType()))
            .str();

    scoped(
        [&]() {
          fn | for_each([&](const llvm::BasicBlock &bb) {
            scoped(
                [&]() {
                  bb | for_each([&](const llvm::Instruction &ins) { walk(&ins); });
                  return true;
                },
                named("BB", bb.getName().str()));
          });
        },
        "Fn: " + sig);
  });
}

std::string agv::LLVMIRTreeVisitor::named(const std::string &kind, const std::string &name) const {
  return normaliseName ? kind : (kind + ": " + name);
}

void agv::LLVMIRTreeVisitor::walk(const llvm::Value *value) {
  if (!value) {
    single("<<<NUll>>>");
    return;
  }
  visitDyn0( //
      value, //
      [&](const llvm::Argument *arg) { return single("Arg: " + print(arg->getType())); },
      [&](const llvm::Instruction *ins) {
        std::string s;
        llvm::raw_string_ostream rso(s);
        ins->print(rso);

        auto args = ins->operands() |
                    mk_string(",", [](const llvm::Use &u) { return print(u->getType()); });
        auto sig = std::string(ins->getOpcodeName()) + "(" + args + "):" + print(ins->getType());
        //        scoped( //
        //            [&]() {
        //              ins->operand_values() | zip_with_index() |
        //                  for_each([&](const llvm::Value *v, auto idx) {
        //                    if (idx == 0) return; //  arg 0 should be the ins itself
        //                    if (v == value) single("(identity)");
        //                    else walk(v);
        //                  });
        //            },
        //            sig);
        single(sig);
      },
      [&](const llvm::Function *c) {
        single(named("Fn", c->getName().str() + ": " + print(c->getType())));
      },
      [&](const llvm::Constant *c) {
        std::string s;
        llvm::raw_string_ostream rso(s);
        c->print(rso);
        single(named("Const", rso.str() + ": " + print(c->getType())));
      },
      [&](const llvm::Value *c) {
        single(named("Val", c->getName().str() + ": " + print(c->getType())));
      }

  );
}
