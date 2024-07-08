#include <iostream>

#include "aspartame/map.hpp"
#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/view.hpp"
#include "p3md/p3md.h"

using namespace llvm;
using namespace aspartame;

enum class Kind : uint8_t { Build = 1, List, Run, Def };

std::map<std::string, std::pair<Kind, std::string>> table = {
    {"build", {Kind::Build, "Build P3MD database"}},
    {"list", {Kind::List, "List entries in a P3MD database"}},
    {"run", {Kind::Run, "Execute Lua scripts against a loaded database"}},
    {"def", {Kind::Def, "Emit Teal type declarations"}},
};

int main(int argc, const char **argv) {

  std::vector<const char *> args(argv, argv + argc);
  auto printHelp = [&]() {
    std::cout << "USAGE: " << args[0] << " <command> <command options>...\n\n"
              << "OPTIONS:\n\n"
              << "  " << std::setw(12) << std::left << "--help"
              << " - Display this help; append this after <command> for command specific help\n\n"
              << "Commands:\n\n";
    table | for_each([](auto arg, auto kv) {
      std::cout << "  " << std::setw(12) << std::left << arg << " - " << kv.second << "\n";
    });
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

  return table ^ get(args[1] ^ to_lower()) ^
         fold(
             [&](Kind kind, auto) {
               auto sliced = args                                          //
                             | zip_with_index()                            //
                             | filter([](auto, auto i) { return i != 1; }) //
                             | keys()                                      //
                             | to_vector();
               switch (kind) {
                 case Kind::Build: return p3md::build_main(sliced.size(), sliced.data());
                 case Kind::List: return p3md::list_main(sliced.size(), sliced.data());
                 case Kind::Run: return p3md::run_main(sliced.size(), sliced.data());
                 case Kind::Def: return p3md::def_main(sliced.size(), sliced.data());
               }
               return EXIT_SUCCESS;
             },
             [&]() {
               std::cout << "Unknown command \"" << command
                         << "\", use --help for a list of commands." << std::endl;
               return EXIT_FAILURE;
             });
}
