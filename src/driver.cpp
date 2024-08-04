#include <iostream>

#include "aspartame/map.hpp"
#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "sv/cli.h"
#include "sv/tool_delta.h"
#include "sv/tool_dump.h"
#include "sv/tool_index.h"
#include "sv/tool_script.h"

using namespace aspartame;

enum class Kind : uint8_t { Index = 1, Dump, Script, Delta };

static bool hasEnv(const std::string &name) {
  if (auto valueCStr = std::getenv(name.c_str()); !valueCStr) return false;
  else if (auto value = std::string(valueCStr) ^ to_lower();
           value == "1" || value == "true" || value == "on" || value == "yes")
    return true;
  else return false;
}

int wrapper(const std::vector<const char *> &args) {

  auto outDir = std::getenv("SV_OUTPUT_DIR");
  if (!outDir) {
    std::cerr << "Database output directory SV_OUTPUT_DIR not set" << std::endl;
    return EXIT_FAILURE;
  }

  auto verbose = hasEnv("SV_VERBOSE");

  // gcc
  //  -###  then count /cc1* or f951 invocations, then do set difference to identify files,
  //   => one file at a time or -MD for dependency
  // Clang
  //  -ccc-print-bindings
  //   => one file at a time or -MD for dependency

  //  std::vector<char *> args{prog};
  //  for (int i = 1; i < argc; ++i)
  //    args.push_back(argv[i]);
  //  args.push_back(nullptr);
  //  execvp(args[0], args.data());
  SV_INFOF("{}", args ^ mk_string(" "));

  //  std::cerr << "execvp failed: " << strerror(errno) << std::endl;
  return EXIT_FAILURE;
}

int main(int argc, const char **argv) {
  std::vector<const char *> args(argv, argv + argc);

  static std::map<std::string, std::pair<Kind, std::string>> table = {
      {sv::index::Name, {Kind::Index, sv::index::Description}},
      {sv::dump::Name, {Kind::Dump, sv::dump::Description}},
      {sv::script::Name, {Kind::Script, sv::script::Description}},
      {sv::delta::Name, {Kind::Delta, sv::delta::Description}},
  };

  auto rightW = 12;
  auto printHelp = [&]() {
    std::cout << "USAGE: " << args[0] << " <command> <command options>...\n\n"
              << "OPTIONS:\n"
              << "  " << std::setw(rightW) << std::left << "--help"
              << " - Display this help; append this after <command> for command specific help\n\n"
              << "Commands:\n";
    table | for_each([&](auto arg, auto kv) {
      std::cout << "  " << std::setw(rightW) << std::left << arg << " - " << kv.second << "\n";
    });
    std::cout << "  " << std::setw(rightW) << std::left << "--" << " - "
              << "Compiler wrapper mode for building SV database" << "\n";
    std::cout << std::endl;
  };

  if (args.empty()) return EXIT_FAILURE;
  if (args.size() < 2) {
    printHelp();
    return EXIT_SUCCESS;
  }

  auto command = args[1] ^ to_lower();
  if (command == "--help" || command == "-help" || command == "help") {
    printHelp();
    return EXIT_SUCCESS;
  }

  // compiler wrapper mode: sv -- ...
  if (args.size() > 1 && std::string(args[1]) == "--") { return wrapper(args ^ drop(2)); }


  return table ^ get(args[1] ^ to_lower()) ^
         fold(
             [&](Kind kind, auto) {
               auto sliced = args                                          //
                             | zip_with_index()                            //
                             | filter([](auto, auto i) { return i != 1; }) //
                             | keys()                                      //
                             | to_vector();
               switch (kind) {
                 case Kind::Index: return sv::index::main(sliced.size(), sliced.data());
                 case Kind::Dump: return sv::dump::main(sliced.size(), sliced.data());
                 case Kind::Script: return sv::script::main(sliced.size(), sliced.data());
                 case Kind::Delta: return sv::delta::main(sliced.size(), sliced.data());
               }
               return EXIT_SUCCESS;
             },
             [&]() {
               std::cout << "Unknown command \"" << command
                         << "\", use --help for a list of commands." << std::endl;
               return EXIT_FAILURE;
             });
}
