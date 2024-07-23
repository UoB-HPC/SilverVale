#include <fstream>
#include <iostream>
#include <utility>

#include "sv/cli.h"
#include "sv/diff.h"
#include "sv/lua.h"
#include "sv/model.h"
#include "sv/par.h"
#include "sv/tool_script.h"

#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;
using namespace llvm;

double tsTree(const sv::Unit &l, const sv::Unit &r) {
  return sv::Diff::apted(l.writtenSource(true).tsTree(), r.writtenSource(true).tsTree());
}

double sTree(const sv::Unit &l, const sv::Unit &r) { return sv::Diff::apted(l.sTree(), r.sTree()); }

double diff(const sv::Unit &l, const sv::Unit &r) {
  return sv::Diff::diff(l.writtenSource(true).content(), r.writtenSource(true).content());
}

static Expected<sv::script::Options> parseOpts(int argc, const char **argv) {
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

  if (auto e = sv::parseCategory(category, argc, argv); e) return std::move(*e);

  return sv::script::Options{.roots = roots | to_vector(),
                             .defs = defs.getValue(),
                             .noBuffer = noBuffer.getValue(),
                             .maxThreads = maxThreads,
                             .args = args | to_vector()};
}

int sv::script::main(int argc, const char **argv) {
  return sv::parseAndRun(argc, argv, &parseOpts, &run);
}

constexpr sv::lua::TypeList< //
    sv::Dependency,          //
    sv::FlatEntry,           //
    sv::ClangEntry,          //
    sv::LLVMBitcode,         //
    sv::Tree,                //
    sv::Source,              //
    sv::Unit,                //
    sv::Codebase,            //
    sv::Database,            //
    sv::Diff,                //
    sv::Glob                 //
    >
    Types;

int bufferedPrint(lua_State *L) {
  AGV_COUT << lua_tostring(L, 1) << std::endl;
  return 0;
}

int sv::script::run(const Options &options) {
  if (options.defs) {
    lua::showTealType(std::cout, lua::bindTeal(Types));
    return EXIT_SUCCESS;
  }
  if (auto script = options.args ^ head_maybe(); !script) {
    AGV_ERRF("No script given, terminating...");
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
