#include <fstream>
#include <iostream>
#include <thread>
#include <utility>

#include "sv/cli.h"
#include "sv/diff.h"
#include "sv/glob.h"
#include "sv/model.h"
#include "sv/par.h"
#include "sv/tool_delta.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/unordered_map.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;
using namespace llvm;
using namespace sv;

size_t longestCommonPrefixLen(const std::vector<std::string> &strings) {
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
using Units = std::vector<std::shared_ptr<Unit>>;
using DiffFn = std::function<double(const Units &, const Units &)>;
using MaxFn = std::function<std::optional<double>(const Units &)>;

struct MemInfo {

  int totalMemoryKB = -1;
  int totalSwapKB = -1;
  int freeSwapKB = -1;
  int availableMemoryKB = -1;

  [[nodiscard]] double swapUsedPCt() const {
    return 1.0 - static_cast<double>(freeSwapKB) / totalSwapKB;
  }

  [[nodiscard]] double ramUsedPct() const {
    return 1.0 - static_cast<double>(availableMemoryKB) / totalMemoryKB;
  }

  static MemInfo read() {
    auto parse = [](const std::string &prefix, const std::string &content) {
      std::size_t start = content.find(prefix);
      if (start != std::string::npos) {
        auto begin = start + prefix.length();
        auto end = content.find("kB", start);
        return std::stoi(content.substr(begin, end - begin));
      }
      return -1;
    };
    if (auto meminfo = std::ifstream("/proc/meminfo"); meminfo.good()) {
      std::string content((std::istreambuf_iterator<char>(meminfo)),
                          std::istreambuf_iterator<char>());
      return MemInfo{
          .totalMemoryKB = parse("MemTotal:", content),
          .totalSwapKB = parse("SwapTotal:", content),
          .freeSwapKB = parse("SwapFree:", content),
          .availableMemoryKB = parse("MemAvailable:", content),
      };
    }
    return {};
  }
};

template <typename F> std::pair<DiffFn, MaxFn> treeSelect(F f) {
  return {
      [=](const Units &lhs, const Units &rhs) -> double {
        return Diff::apted(Tree::combine("root", lhs ^ map([&](auto &x) { return f(x); })),
                           Tree::combine("root", rhs ^ map([&](auto &x) { return f(x); })));
      },
      [=](const Units &xs) -> std::optional<double> {
        return Tree::combine("root", xs ^ map([&](auto &x) { return f(x); })).nodes();
      },
  };
};

double minDiff(std::vector<std::string> &&ls, std::vector<std::string> &&rs) {
  auto value = std::numeric_limits<double>::max();
  do {
    auto lhsSrc = ls ^ fold_left(std::string{}, [](auto &&acc, auto &x) { return acc += x; });
    do {
      auto rhsSrc = rs ^ fold_left(std::string{}, [](auto &&acc, auto &x) { return acc += x; });
      value = std::fmin(value, static_cast<double>(Diff::diff(lhsSrc, rhsSrc)));
    } while (std::next_permutation(rs.begin(), rs.end()));
  } while (std::next_permutation(ls.begin(), ls.end()));
  return value;
}

std::pair<DiffFn, MaxFn> createTask(delta::TaskDesc desc) {
  switch (desc.kind) {
    // source cases
    case delta::Kind::SLOCAbs:
      return {
          [=](const Units &, const Units &rhs) -> double {
            return rhs ^ fold_left(0, [&](auto acc, auto &u) {
                     switch (desc.mod) {
                       case delta::Modifier::Raw: return acc + u->sourceAsWritten().sloc();
                       case delta::Modifier::CPP: return acc + u->sourcePreprocessed().sloc();
                       case delta::Modifier::Cov: return acc + u->sourceWithCoverage().sloc();
                     }
                   });
          },
          [](const Units &) -> std::optional<double> { return {}; },
      };
    case delta::Kind::LLOCAbs:
      return {
          [=](const Units &, const Units &rhs) -> double {
            return rhs ^ fold_left(0, [&](auto acc, auto &u) {
                     switch (desc.mod) {
                       case delta::Modifier::Raw: return acc + u->sourceAsWritten().lloc();
                       case delta::Modifier::CPP: return acc + u->sourcePreprocessed().lloc();
                       case delta::Modifier::Cov: return acc + u->sourceWithCoverage().lloc();
                     }
                   });
          },
          [](const Units &) -> std::optional<double> { return {}; },
      };
    case delta::Kind::SrcLenAbs: {
      return {
          [=](const Units &, const Units &rhs) -> double {
            return rhs ^ fold_left(0, [&](auto acc, auto &u) {
                     switch (desc.mod) {
                       case delta::Modifier::Raw:
                         return acc + u->sourceAsWritten().contentWhitespaceNormalised().size();
                       case delta::Modifier::CPP:
                         return acc + u->sourcePreprocessed().contentWhitespaceNormalised().size();
                       case delta::Modifier::Cov:
                         return acc + u->sourceWithCoverage().contentWhitespaceNormalised().size();
                     }
                   });
          },
          [](const Units &) -> std::optional<double> { return {}; },
      };
    }
    case delta::Kind::SrcRel: {
      auto extract = [](delta::Modifier m, const Units &xs) {
        return xs ^ map([=](auto &x) {
                 switch (m) {
                   case delta::Modifier::Raw:
                     return x->sourceAsWritten().contentWhitespaceNormalised();
                   case delta::Modifier::CPP:
                     return x->sourcePreprocessed().contentWhitespaceNormalised();
                   case delta::Modifier::Cov:
                     return x->sourceWithCoverage().contentWhitespaceNormalised();
                 }
               });
      };
      return {
          [=](const Units &lhs, const Units &rhs) -> double {
            return minDiff(extract(desc.mod, lhs), extract(desc.mod, rhs));
          },
          [=](const Units &xs) -> std::optional<double> {
            return (extract(desc.mod, xs)                    //
                    | map([](auto &&x) { return x.size(); }) //
                    | reduce(std::plus<>()))                 //
                .value_or(0);                                //
          },
      };
    }
    case delta::Kind::TSTreeRel:
      return treeSelect([=](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: return u->sourceAsWritten().tsTree();
          case delta::Modifier::CPP: return u->sourcePreprocessed().tsTree();
          case delta::Modifier::Cov: return u->sourceWithCoverage().tsTree();
        }
      });
    // semantic cases
    case delta::Kind::STreeRel:
      return treeSelect([=](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: [[fallthrough]];
          case delta::Modifier::CPP: return u->sTree(Unit::View::AsIs);
          case delta::Modifier::Cov: return u->sTree(Unit::View::WithCoverage);
        }
      });
    case delta::Kind::STreeInlineRel:
      return treeSelect([=](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: [[fallthrough]];
          case delta::Modifier::CPP: return u->sTreeInlined(Unit::View::AsIs);
          case delta::Modifier::Cov: return u->sTreeInlined(Unit::View::WithCoverage);
        }
      });
    case delta::Kind::IRTreeRel:
      return treeSelect([=](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: [[fallthrough]];
          case delta::Modifier::CPP: return u->irTree(Unit::View::AsIs);
          case delta::Modifier::Cov: return u->irTree(Unit::View::WithCoverage);
        }
      });
  }
}

Expected<delta::Options> parseOpts(int argc, const char **argv) {
  using namespace delta;
  static cl::OptionCategory category("Diff options");

  // streeRel, streeInlinedRel, irtreeRel (+raw, +cov)
  // slocAbs, llocAbs, tstreeRel, sourceRel (modifiers=(+raw, +cpp, +cov)

  static cl::list<std::string> kinds(
      "kinds", cl::CommaSeparated, cl::OneOrMore,
      cl::desc(
          "Comma separated kinds of metric to use for diff operation. Defaults to all "
          "supported kinds.\n"
          "Source-based:\n"
          "  sloc         - Source Lines of Code (Absolute measure).\n"
          "  lloc         - Logical Lines of Code (Absolute measure).\n"
          "  source       - Source code (whitespace normalised edit difference).\n"
          "  tstree       - Tree-sitter tree.\n"
          "Semantic-based:\n"
          "  stree        - AST based (e.g ClangAST/High GIMPLE, etc) semantic tree with "
          "  symbols normalised.\n"
          "  streeinlined - AST based semantic tree with symbols normalised and calls inlined.\n"
          "  irtree       - IR based (e.g LLVM IR/Low GIMPLE) semantic tree with symbols "
          "normalised.\n"
          "\nBy default the metric uses the source code or semantic as written, the following "
          "modifiers for each kind are supported:"
          "  +raw - Source code or semantic as written, no effect\n"
          "  +cpp - Run the diff after the preprocessor has finished execution (source based kinds "
          "only)\n"
          "  +cov - Run the diff after pruning out code that was not covered if coverage is "
          "available in the database\n"));

  static cl::opt<std::string> root(
      "root",
      cl::desc("Root path shared by all databases."
               "Analysis (delta) will not escape the unions of all root paths."),
      cl::Optional);

  static cl::opt<std::string> outputPrefix(
      "output", cl::desc("The output file name prefix for all result CSV files."), cl::Optional);

  static cl::opt<int> maxThreads( //
      "j",
      cl::desc(
          "Number of parallel AST frontend jobs in parallel, defaults to total number of threads."),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category));

  static cl::list<std::string> excludes( //
      "excludes", cl::OneOrMore, cl::PositionalEatsArgs,
      cl::desc("<TU glob> Exclude TUs that match the TU glob."), cl::cat(category));

  static cl::list<std::string> merges( //
      "merges", cl::OneOrMore, cl::PositionalEatsArgs,
      cl::desc("<TU glob>:<TU name> Combine multiple TUs (files) into a single TU where all TUs "
               "matching TU glob will be merged"),
      cl::cat(category));

  static cl::list<std::string> matches( //
      "matches", cl::ZeroOrMore, cl::PositionalEatsArgs,
      cl::desc(
          "A comma separated glob pairs for matching TUs (files) against the base TUs."
          "The format is <base glob>:<model glob>,... where the diff will run on the first "
          "matching pattern pair; malformed patterns are ignored."
          "This overrides the default behaviour where TUs are matched by identical filenames."),
      cl::cat(category));

  static cl::opt<std::string> base( //
      "base",
      cl::desc("The base database to compare against, defaults to the first positional database "
               "argument."),
      cl::Optional);

  static cl::list<std::string> databases( //
      cl::FormattingFlags::Positional,
      cl::desc("<database> [... <database N>]\n"
               "  Compare all databases against the base database.\n"
               "  Each database can optionally be suffixed by a colon separated list of root paths "
               "(e.g db_path_1:rootA:rootB).\n"
               "  When applicable, tree analysis will not escape the union of all root paths."
               "The base database can be specified via --base <database>"),
      cl::cat(category));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  auto pairPattern = [](const std::string &in,
                        char delim) -> std::optional<std::pair<std::string, std::string>> {
    auto pair = in ^ split(delim);
    if (pair.size() == 2) return std::pair{pair[0], pair[1]};
    AGV_WARNF("Ignoring malformed pair pattern with delimiter ({}): `{}`", delim, in);
    return std::nullopt;
  };

  auto mappedDbs =
      databases | map([](auto &x) {
        auto paths = x ^ split(":");
        return DatabaseSpec{paths ^ head_maybe() ^ get_or_else(x),
                            root.empty() ? paths ^ tail() : paths ^ tail() ^ append(root)};
      }) |
      to_vector();

  return Options{
      .databases = mappedDbs,
      .base = base.empty() ? mappedDbs ^ head_maybe() ^
                                 fold([](auto &d) { return d.path.string(); }, []() { return ""; })
                           : base,
      .kinds = kinds | collect([](auto &spec) -> std::optional<TaskDesc> {
                 auto parts = spec ^ split("+");
                 if (parts.size() == 1) {
                   if (auto k = delta::parseKind(parts[0]); k)
                     return TaskDesc{*k, delta::Modifier::Raw};

                 } else if (parts.size() == 2) {
                   auto k = delta::parseKind(parts[0]);
                   auto m = delta::parseModifier(parts[1]);
                   if (k && m) return TaskDesc{*k, *m};
                 }

                 AGV_WARNF("Ignoring malformed kind: {} (parsed as [{}])", spec,
                           parts ^ mk_string(","));
                 return std::nullopt;
               }) //
               | to_vector(),
      .excludes = excludes | map([](auto &s) { return ExcludeFilter{s}; }) | to_vector(),
      .merges = merges | collect([&](auto &s) {
                  return pairPattern(s, ':') ^ map([&](auto &glob, auto &name) {
                           return EntryMerge{.glob = glob, .name = name};
                         });
                }) |
                to_vector(),
      .matches = matches | collect([&](auto &p) { return pairPattern(p, ':'); }) |
                 map([](auto &source, auto &target) { //
                   return EntryMatch{.sourceGlob = source, .targetGlob = target};
                 }) |
                 to_vector(),
      .outputPrefix = outputPrefix.getValue(),
      .maxThreads = maxThreads};
}

int delta::main(int argc, const char **argv) { return parseAndRun(argc, argv, &parseOpts, &run); }

int delta::run(const delta::Options &options) {

  tbb::global_control global_limit(tbb::global_control::max_allowed_parallelism,
                                   options.maxThreads);

  if (options.databases.empty()) {
    AGV_ERRF("At least 1 database required for comparison");
    return EXIT_FAILURE;
  }

  // P3MD_COUT << "# Using base glob pattern: " << (options.baseGlobs ^ mk_string(", ")) <<
  // std::endl; P3MD_COUT << "# Using pair glob pattern: "
  //           << (options.entryGlobPairs.empty()
  //                   ? "(filename match)"
  //                   : (options.entryGlobPairs ^
  //                      mk_string(", ", [](auto &l, auto &r) { return l + " -> " + r; })))
  //           << std::endl;

  AGV_COUT << "# Loading " << options.databases.size() << " models ..." << std::endl;

  struct Model {
    std::string path;
    std::vector<std::pair<std::string, Units>> entries;
  };

  std::vector<Model> models = par_map(options.databases, [&](auto &spec) {
    const auto excludes = options.excludes ^ map([](auto &f) { return globToRegex(f.glob); });
    const Database db = Codebase::loadDB(spec.path);
    const Codebase cb = Codebase::load(db, std::cout, true, {}, [&](auto &path) {
      return excludes ^ forall([&](auto &r) { return !std::regex_match(path, r); });
    });

    std::cout << "> " << cb.root << std::endl;
    auto root = std::filesystem::current_path() /
                fmt::format("debug_{}", std::filesystem::path(cb.root).filename());
    std::filesystem::create_directories(root);

    {
      std::ofstream out(root / "cov.all.txt");
      auto xss = (cb.coverage->entries | to_vector())                                            //
                 ^ map([](auto &p, auto count) { return std::tuple{p.first, p.second, count}; }) //
                 ^ sort();
      for (auto &[name, line, count] : xss)
        out << name << ":" << line << "=" << count << "\n";
    }
    for (auto &u : cb.units) {
      auto path = std::filesystem::path(u->name());
      auto stem = path.stem(), ext = path.extension();
      {
        std::ofstream out(root / fmt::format("{}.raw{}", stem, ext));
        out << u->sourceAsWritten().contentWhitespaceNormalised();
      }
      {
        std::ofstream out(root / fmt::format("{}.cpp{}", stem, ext));
        out << u->sourcePreprocessed().contentWhitespaceNormalised();
      }
      {
        std::ofstream out(root / fmt::format("{}.cov{}", stem, ext));
        out << u->sourceWithCoverage().contentWhitespaceNormalised();
      }
      {
        std::ofstream out(root / fmt::format("{}.raw.txt", stem));
        out << u->sourceAsWritten().tsTree().prettyPrint();
      }
      {
        std::ofstream out(root / fmt::format("{}.cpp.txt", stem));
        out << u->sourcePreprocessed().tsTree().prettyPrint();
      }
      {
        std::ofstream out(root / fmt::format("{}.cov.txt", stem));
        out << u->sourceWithCoverage().tsTree().prettyPrint();
      }
    }

    const auto merges =
        options.merges ^ map([](auto &m) { return std::pair{globToRegex(m.glob), m.name}; });
    Model model{
        .path = cb.root,
        .entries =                                                                            //
        cb.units                                                                              //
        ^ group_by([&](auto &u) {                                                             //
            return (merges                                                                    //
                    | filter([&](auto &r, auto &) { return std::regex_match(u->path(), r); }) //
                    | values()                                                                //
                    | head_maybe()) ^                                                         //
                   get_or_else(u->name());                                                    //
          })                                                                                  //
        ^ to_vector() ^ sort_by([](auto &name, auto &us) {                                    //
            return std::pair{name,                                                            //
                             us ^ fold_left(0, [](auto acc, auto &u) {
                               return acc + u->sourceAsWritten().sloc();
                             })};
          })};
    std::cout << "# [ " << model.path << " ]" << std::endl;
    for (auto &[name, us] : model.entries) {
      std::cout << "# " << name << " -> {"
                << (us ^ mk_string(", ", [](auto &u) { return u->path(); })) << "\n";
    }
    return model;
  });

  AGV_INFOF("All models loaded");

  struct Key {
    delta::TaskDesc desc{};
    std::string name{};
    size_t modelIdx{};
  };

  using DiffTask =
      std::pair<Key, std::variant<std::tuple<DiffFn, Units, Units>, std::tuple<MaxFn, Units>>>;

  if (auto ls = models ^ head_maybe(); ls) {
    std::vector<DiffTask> deltaTasks;
    options.kinds                                                           //
        | map([](auto &desc) { return std::pair{desc, createTask(desc)}; }) //
        | for_each([&](auto &desc, auto &p) {
            auto [delta, max] = p;
            ls->entries | for_each([&](auto &lhsName, const Units &l) {
              auto addTask = [&](size_t idx, auto fn) {
                deltaTasks.emplace_back(
                    std::pair{Key{.desc = desc, .name = lhsName, .modelIdx = idx}, fn});
              };
              models | zip_with_index() | for_each([&](auto &r, size_t idx) { //
                Key key{.desc = desc, .name = lhsName, .modelIdx = idx};
                r.entries                                                               //
                    | filter([&](auto &rhsName, auto &) { return lhsName == rhsName; }) //
                    | for_each([&](auto &, const Units &r) {                            //
                        deltaTasks.emplace_back(key, std::tuple{delta, l, r});
                        deltaTasks.emplace_back(key, std::tuple{max, r});
                      });
              });
            });
          });

    AGV_INFOF("Created {} tasks", deltaTasks.size());

    auto cost = [](auto &&xs) {
      return xs | fold_left(0, [](auto acc, auto &u) {
               return acc                                         //
                      + u->sTree(Unit::View::AsIs).nodes()        //
                      + u->sTreeInlined(Unit::View::AsIs).nodes() //
                      + u->irTree(Unit::View::AsIs).nodes();      //
             });
    };
    auto taskReverseSizes = deltaTasks ^ sort_by([&](auto &, auto v) {
                              return v ^ fold_total(
                                             [&](const std::tuple<DiffFn, Units, Units> &t) {
                                               auto [_, lhs, rhs] = t;
                                               return -cost(lhs | concat(rhs));
                                             },
                                             [&](const std::tuple<MaxFn, Units> &t) {
                                               auto [_, xs] = t;
                                               return -cost(xs);
                                             });
                            });

    auto logger = ProgressLogger{deltaTasks.size(),
                                 deltaTasks ^ fold_left(int{}, [](auto acc, auto &t) {
                                   return std::max(acc, static_cast<int>(t.first.name.size()));
                                 })};

    Cached<MemInfo> memInfo(std::chrono::milliseconds(100));

    double limit = 0.95;
    using Result = std::pair<Key, std::variant<double, std::optional<double>>>;
    auto results = par_map(taskReverseSizes, [&](auto &task) {
      auto usedPct = memInfo(&MemInfo::read).ramUsedPct();
      while (usedPct > limit) {
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        usedPct = memInfo(&MemInfo::read).ramUsedPct();
      }
      auto &[key, fn] = task;
      logger.log(key.name);
      return fn ^ fold_total(
                      [&](const std::tuple<DiffFn, Units, Units> &t) -> Result {
                        auto [f, l, r] = t;
                        return std::pair{key, f(l, r)};
                      },
                      [&](const std::tuple<MaxFn, Units> &t) -> Result {
                        auto [f, xs] = t;
                        return std::pair{key, f(xs)};
                      });
    });

    auto tabulate = [&]<typename T>(const std::vector<std::pair<Key, T>> &xs, auto f) {
      size_t prefixLen = longestCommonPrefixLen(models ^ map([&](auto &m) { return m.path; }));
      return (xs ^ group_map([](auto &k, auto) { return k.modelIdx; },                       //
                             [](auto &k, auto v) { return std::tuple{k.desc, k.name, v}; })) //
             ^ to_vector()                                                                   //
             ^ sort_by([](auto &modelIdx, auto) { return modelIdx; })                        //
             ^ bind([&](auto &modelIdx, auto &xs) {                                          //
                 auto modelName = models[modelIdx].path.substr(prefixLen);
                 return xs //
                        ^ sort_by([](auto &desc, auto &name, auto) {
                            return std::tuple{desc.kind, desc.mod, name};
                          }) //
                        ^ map([&](auto &desc, auto &name, auto &value) {
                            auto category =
                                fmt::format("{}+{}", to_string(desc.kind), to_string(desc.mod));

                            return modelIdx == 0 ? std::vector{category, name,              //
                                                               f(value)}                    //
                                                 : std::vector{f(value)};                   //
                          })                                                                //
                        ^ prepend(modelIdx == 0                                             //
                                      ? std::vector<std::string>{"kind", "name", modelName} //
                                      : std::vector{modelName})                             //
                        ^ transpose();
               }) //
             ^ transpose();
    };

    auto diffs = results ^ collect([](auto k, auto v) {
                   return v ^ get<double>() ^ map([&](auto v) { return std::pair{k, v}; });
                 });

    auto maxes =
        results ^ collect([](auto k, auto v) {
          return v ^ get<std::optional<double>>() ^ map([&](auto v) { return std::pair{k, v}; });
        });

    std::cout << "\n#max\n";
    auto m = tabulate(maxes, [](auto v) { return std::to_string(v.value_or(-1)); });
    for (auto row : m) {
      AGV_COUT << (row ^ mk_string(", ")) << std::endl;
    }
    std::cout << "\n#delta\n";
    auto d = tabulate(diffs, [](auto v) { return std::to_string(v); });
    for (auto row : d) {
      AGV_COUT << (row ^ mk_string(", ")) << std::endl;
    }

    // MEMORY
    // SAVE FORMAT

    //    m.x;

    //              model
    // kind, entry
  }

  return EXIT_SUCCESS;
}
