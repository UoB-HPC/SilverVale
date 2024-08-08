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
#include "clipp.h"

using namespace aspartame;

int sv::script::main(int argc, char **argv) {
  using namespace clipp;
  bool help{};
  Options opts{};
  opts.maxThreads = static_cast<int>(std::thread::hardware_concurrency());

  auto cli = ( //
      option("-h", "--help").set(help).doc("Show help"),

      option("--defs").set(opts.defs) //
          % "Emit Teal type (*.d.tl) declarations to standard output for Teal script development. "
            "This option supersedes any other argument or user script.",

      option("--nobuffer").set(opts.noBuffer) //
          % "Disable buffering to C++'s stdout for print.",

      option("--threads", "-j")                                                                  //
              % "Global number of jobs to run in parallel, defaults to total number of threads." //
          & value("threads", opts.maxThreads),

      option("--roots")                                                              //
              % "Additional search paths in CSV format for Lua's requires function." //
          & value("roots", opts.roots),

      values("scripts", opts.args) //
          % "The path to the Lua user script followed by arguments passed to the arg table for "
            "the script.");

  if (!parse(argc, argv, cli) || help) {
    std::cerr << clipp::make_man_page(cli, argv[0]) << std::endl;
    return EXIT_FAILURE;
  }

  return run(opts);
}

constexpr sv::lua::TypeList<       //
    sv::Dependency,                //
    sv::Tree,                      //
    sv::Source,                    //
    sv::Unit,                      //
    sv::Codebase,                  //
    sv::PerFileCoverage,           //
    sv::PerFileCoverage::Instance, //
    sv::Database,                  //
    sv::Database::Entry,           //
    sv::Diff,                      //
    sv::Glob                       //
    >
    Types;

int bufferedPrint(lua_State *L) {
  SV_COUT << lua_tostring(L, 1) << std::endl;
  return 0;
}

int sv::script::run(const Options &options) {
  if (options.defs) {
    lua::showTealType(std::cout, lua::bindTeal(Types));
    return EXIT_SUCCESS;
  }
  if (auto script = options.args ^ head_maybe(); !script) {
    SV_ERRF("No script given, terminating...");
    return EXIT_FAILURE;
  } else {
    par_setup(options.maxThreads);
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
