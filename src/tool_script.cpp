#include <fstream>
#include <iostream>
#include <utility>

#include "agv/cli.h"
#include "agv/diff.h"
#include "agv/lua.h"
#include "agv/model.h"
#include "agv/par.h"
#include "agv/tool_script.h"

#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;
using namespace llvm;

double tsTree(const agv::Unit &l, const agv::Unit &r) {
  return agv::Diff::apted(l.source(true).tsTree(), r.source(true).tsTree());
}

double sTree(const agv::Unit &l, const agv::Unit &r) {
  return agv::Diff::apted(l.sTree(), r.sTree());
}

double diff(const agv::Unit &l, const agv::Unit &r) {
  return agv::Diff::diff(l.source(true).content(), r.source(true).content());
}

static Expected<agv::script::Options> parseOpts(int argc, const char **argv) {
  static cl::OptionCategory category("Run options");

  static cl::opt<bool> defs( //
      "defs",
      cl::desc(
          "Emit Teal type (*.d.tl) declarations to standard output for Teal script development."
          "This option supersedes any other argument or user script."),
      cl::cat(category));

  static cl::opt<bool> noBuffer( //
      "no-buffer", cl::desc("Disable buffering to C++'s stdout for print"), cl::cat(category));

  static cl::list<std::string> roots(
      cl::CommaSeparated, "roots",
      cl::desc("Additional search paths in CSV format for Lua's requires function."),
      cl::cat(category));

  static cl::opt<int> maxThreads(
      "j",
      cl::desc("Global number of jobs to run in parallel, defaults to total number of threads."),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category));

  static cl::list<std::string> args( //
      cl::FormattingFlags::Positional,
      cl::desc("<script> [... args]\n"
               "The path to the Lua user script followed by arguments passed to the arg table for "
               "the script."),
      cl::cat(category));

  if (auto e = agv::parseCategory(category, argc, argv); e) return std::move(*e);

  return agv::script::Options{.roots = roots | to_vector(),
                              .defs = defs.getValue(),
                              .noBuffer = noBuffer.getValue(),
                              .maxThreads = maxThreads,
                              .args = args | to_vector()};
}

int agv::script::main(int argc, const char **argv) {
  return agv::parseAndRun(argc, argv, &parseOpts, &run);
}

constexpr agv::lua::TypeList<       //
    agv::FlatDatabase,              //
    agv::FlatDatabase::Entry,       //
    agv::ClangDatabase,             //
    agv::ClangDatabase::Entry,      //
    agv::ClangDatabase::Bitcode,    //
    agv::ClangDatabase::Dependency, //
    agv::Tree,                      //
    agv::Source,                    //
    agv::Unit,                      //
    agv::Codebase,                  //
    agv::Databases,                 //
    agv::Diff,                      //
    agv::Glob                       //
    >
    Types;

int bufferedPrint(lua_State *L) {
  AGV_COUT << lua_tostring(L, 1) << std::endl;
  return 0;
}

int agv::script::run(const Options &options) {
  if (options.defs) {
    lua::showTealType(std::cout, lua::bindTeal(Types));
    return EXIT_SUCCESS;
  }
  if (auto script = options.args ^ head_maybe(); !script) {
    std::cerr << "No script given, terminating..." << std::endl;
    return EXIT_FAILURE;
  } else {
    auto global_limit = par_setup(options.maxThreads);
    sol::state state = lua::bindSol(Types);
    if (!options.roots.empty()) {
      state["package"]["path"] = std::string(options.roots | mk_string(";")) + ";" +
                                 state["package"]["path"].get<std::string>();
    }
    state["arg"] = sol::as_table(options.args ^ tail());
    if (!options.noBuffer) { state.set_function("print", &bufferedPrint); }
    auto status = state.script_file(*script).status();
    return status == sol::call_status::ok
               ? EXIT_SUCCESS
               : static_cast<std::underlying_type_t<sol::call_status>>(status);
  }
}
