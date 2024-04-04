#include "aspartame/map.hpp"
#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/view.hpp"
#include "p3md/p3md.h"
#include "llvm/Support/CommandLine.h"
#include <iostream>

using namespace llvm;
using namespace aspartame;

enum class Kind : uint8_t {
  Build = 1,
  List,
  DiffSTree,
  DiffSource,
  DiffTSNode,
  DumpSTree,
  DumpSource,
  DumpTSNode,
};

std::map<std::string, std::pair<Kind, std::string>> table = {
    {"build", {Kind::Build, "Build P3MD database"}},
    {"list", {Kind::List, "List entries in a P3MD database"}},
    {"diff-stree", {Kind::DiffSTree, "Diff two P3MD database's stree data of selected entries"}},
    {"diff-source", {Kind::DiffSource, "Diff two P3MD database's source data of selected entries"}},
    {"diff-tsnode", {Kind::DiffTSNode, "Diff two P3MD database's tsnode data of selected entries"}},
    {"dump-stree", {Kind::DumpSTree, "Dump a P3MD database's stree data of selected entries"}},
    {"dump-source", {Kind::DumpSource, "Dump a P3MD database's source data of selected entries"}},
    {"dump-tsnode", {Kind::DumpTSNode, "Dump a P3MD database's tsnode data of selected entries"}}};

static cl::list<std::string> Argv(cl::ConsumeAfter, cl::desc("<program arguments>..."));

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
                 case Kind::DiffSTree:
                   return p3md::diff_main(p3md::DataKind::STree, sliced.size(), sliced.data());
                 case Kind::DiffSource:
                   return p3md::diff_main(p3md::DataKind::Source, sliced.size(), sliced.data());
                 case Kind::DiffTSNode:
                   return p3md::diff_main(p3md::DataKind::TSNode, sliced.size(), sliced.data());
                 case Kind::DumpSTree:
                   return p3md::dump_main(p3md::DataKind::STree, sliced.size(), sliced.data());
                 case Kind::DumpSource:
                   return p3md::dump_main(p3md::DataKind::Source, sliced.size(), sliced.data());
                 case Kind::DumpTSNode:
                   return p3md::dump_main(p3md::DataKind::TSNode, sliced.size(), sliced.data());
               }
               return EXIT_SUCCESS;
             },
             [&]() {
               std::cout << "Unknown command \"" << command
                         << "\", use --help for a list of commands." << std::endl;
               return EXIT_FAILURE;
             });

  return EXIT_SUCCESS;
}
