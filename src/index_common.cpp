#include <filesystem>
#include <fstream>
#include <iostream>

#include "sv/cli.h"
#include "sv/database.h"
#include "sv/exec.h"
#include "sv/index_common.h"
#include "llvm/Support/Program.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace llvm;
using namespace aspartame;

std::optional<std::pair<std::filesystem::path, std::string>>
sv::resolveProgramAndDetect(const std::filesystem::path &program,
                            const std::function<bool(const std::string &)> &predicate,
                            const std::unordered_map<std::string, std::string> &programLUT) {
  auto resolved = program;
  if (!program.is_absolute()) {
    if (auto it = programLUT.find(program); it != programLUT.end()) { resolved = it->second; }
  }
  std::stringstream output;
  if (auto code = sv::exec(fmt::format("{} --version", program), output); !code || *code != 0)
    return {};
  return ((output.str() ^ lines())                                                           //
          | collect([&](auto &l) { return predicate(l) ? std::optional{l} : std::nullopt; }) //
          | head_maybe())                                                                    //
         ^ map([&](auto &version) { return std::pair{resolved, version}; });
}

std::string sv::readFile(const std::filesystem::path &file) {
  std::ifstream t(file);
  t.exceptions(std::ios::badbit | std::ios::failbit);
  std::stringstream buffer;
  buffer << t.rdbuf();
  return buffer.str();
}

std::vector<std::string> sv::stripHeadAndOArgs(const std::vector<std::string> &args) {
  return args                                                                               //
         | zip_with_index()                                                                 //
         | bind([](auto &s, auto idx) {                                                     //
             if (s == "-o") return std::vector<size_t>{idx, idx + 1};                       //
             else if (s ^ starts_with("-o")) return std::vector<size_t>{idx};               //
             return std::vector<size_t>{};                                                  //
           })                                                                               //
         | and_then([&](auto x) {                                                           //
             std::unordered_set<size_t> discardIndices(x.begin(), x.end());                 //
             return args                                                                    //
                    | zip_with_index()                                                      //
                    | filter([&](auto, auto idx) { return !discardIndices.contains(idx); }) //
                    | keys()                                                                //
                    | tail()                                                                //
                    | to_vector();
           });
}

std::map<std::string, sv::Dependency> sv::readDepFile(const std::filesystem::path &depFile,
                                                      const std::string &source) {
  std::map<std::string, sv::Dependency> deps;
  auto addDep = [&](const std::filesystem::path &file) {
    try {
      auto time = std::chrono::system_clock::to_time_t(       //
          std::chrono::clock_cast<std::chrono::system_clock>( //
              std::filesystem::last_write_time(file)));
      deps.emplace(file, sv::Dependency{time, readFile(file)});
    } catch (const std::exception &e) { AGV_WARNF("cannot read/stat dependency {}: {}", file, e); }
  };
  addDep(source);

  const auto stem = std::filesystem::path(source).stem();
  std::ifstream stream(depFile);
  std::string line;
  while (std::getline(stream, line)) {
    if (line ^ starts_with(stem)) continue;
    line                                             //
        ^ filter([](auto &x) { return x != '\\'; })  //
        ^ trim()                                     //
        ^ split(' ')                                 //
        ^ map([](auto &f) { return f ^ trim(); })    //
        ^ filter([](auto &f) { return !f.empty(); }) //
        ^ for_each([&](auto &f) { addDep(f); });     //
  }
  return deps;
}