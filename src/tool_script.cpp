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
#include "cxxopts.hpp"

using namespace aspartame;

int sv::script::main(int argc, const char **argv) {
  cxxopts::Options options(Name, Description);
  options.add_options() //
      ("defs",
       "Emit Teal type (*.d.tl) declarations to standard output for Teal script development."
       "This option supersedes any other argument or user script.",
       cxxopts::value<bool>()->default_value("false")) //
      ("nobuffer", "Disable buffering to C++'s stdout for print.",
       cxxopts::value<bool>()->default_value("true")) //
      ("j,threads",
       "Global number of jobs to run in parallel, defaults to total number of threads.",
       cxxopts::value<int>()->default_value(std::to_string(std::thread::hardware_concurrency()))) //
      ("roots", "Additional search paths in CSV format for Lua's requires function.",
       cxxopts::value<std::vector<std::string>>()) //
      ("scripts",
       "The path to the Lua user script followed by arguments passed to the arg table for "
       "the script.",
       cxxopts::value<std::vector<std::string>>());

  options.parse_positional({"scripts"});
  auto result = options.parse(argc, argv);
  if (result.count("help")) {
    SV_COUT << options.help() << std::endl;
    return EXIT_SUCCESS;
  } else
    return run(Options{.roots = result["roots"].as<std::vector<std::string>>(),
                       .defs = result["defs"].as<bool>(),
                       .noBuffer = result["nobuffer"].as<bool>(),
                       .maxThreads = result["threads"].as<int>(),
                       .args = result["scripts"].as<std::vector<std::string>>()});
}

constexpr sv::lua::TypeList<       //
    sv::Dependency,                //
    sv::FlatEntry,                 //
    sv::ClangEntry,                //
    sv::LLVMBitcode,               //
    sv::Tree,                      //
    sv::Source,                    //
    sv::Unit,                      //
    sv::Codebase,                  //
    sv::PerFileCoverage,           //
    sv::PerFileCoverage::Instance, //
    sv::Database,                  //
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
