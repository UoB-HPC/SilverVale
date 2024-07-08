#include <fstream>
#include <shared_mutex>
#include <utility>

#include "p3md/database.h"
#include "p3md/diff.h"
#include "p3md/glob.h"
#include "p3md/lua.h"
#include "p3md/par.h"
#include "p3md/run.h"
#include "p3md/term.h"
#include "p3md/tree.h"

#include "llvm/Support/MemoryBuffer.h"

#include "aspartame/string.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;

static size_t longestCommonPrefixLength(const std::vector<std::string> &strings) {
  if (strings.size() < 2) return 0;
  auto sorted = strings ^ sort();
  auto &first = sorted.front();
  auto &last = sorted.back();
  size_t len = std::min(first.length(), last.length());
  size_t i = 0;
  while (i < len && first[i] == last[i])
    i++;
  return i;
}

class ProgressLogger {
  size_t total;
  int maxLogLength;
  std::atomic_size_t completed{1};

public:
  ProgressLogger(size_t total, int maxLogLength) : total(total), maxLogLength(maxLogLength) {}
  void log(const std::string &line, bool progress = true) {
    auto s = P3MD_COUT << "# [" << (progress ? completed++ : completed.load()) << "/" << total
                       << "] " << std::left << std::setw(maxLogLength + 10) << line;
    if (progress) (s << "\r").flush();
    else s << std::endl;
  }
};


double tsTree(const p3md::Unit &l, const p3md::Unit &r){
  return p3md::Diff::apted(l.source(true).tsTree(), r.source(true).tsTree());
}

double sTree(const p3md::Unit &l, const p3md::Unit &r){
  return p3md::Diff::apted(l.sTree(), r.sTree());
}

double diff(const p3md::Unit &l, const p3md::Unit &r){
  return p3md::Diff::diff(l.source(true).content(), r.source(true).content());
}

int p3md::run::run(const p3md::run::Options &options) {

  auto global_limit = p3md::par_setup(options.maxThreads);

//  ProgressLogger logger(options.databases.size(),
//                        options.databases | fold_left(0, [](auto acc, auto &x) {
//                          return std::max(0, static_cast<int>(x.length()));
//                        }));
//
//  std::vector<std::optional<Database>> databases(options.databases.size());
//  par_for(options.databases, [&](auto &path, size_t idx) {
//    logger.log(path);
//    auto buffer = llvm::MemoryBuffer::getFile(path + "/db.json", /*IsText*/ true);
//    if (auto e = buffer.getError()) {
//      std::cerr << "Skipping " << path << " due to IO error: " << e.message() << std::endl;
//    } else databases[idx] = p3md::Database::fromJson((*buffer)->getBuffer().str());
//  });
//  P3MD_COUT << std::endl;


  sol::state state = lua::createState();



  if (!options.scriptRoots.empty()) {
    state["package"]["path"] = std::string(options.scriptRoots | mk_string(";")) + ";" +
                               state["package"]["path"].get<std::string>();
  }

  lua::bind(state, lua::Types);

  state["arg"] = sol::as_table(options.args); //  databases ^ collect(std::identity());

  //  databases | collect(std::identity()) | for_each([](auto x) { std::cout << x << "\n"; });

  state.script_file(options.script);

//  if (options.databases.empty()) {
//    std::cerr << "At least one database required" << std::endl;
//    return EXIT_FAILURE;
//  }

  return EXIT_SUCCESS;
}
