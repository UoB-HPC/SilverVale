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
#include "clipp.h"

using namespace aspartame;
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
}

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
          [=](const Units &lhs, const Units &) -> double {
            return lhs ^ fold_left(0, [&](auto acc, auto &u) {
                     switch (desc.mod) {
                       case delta::Modifier::Raw: return acc + u->sourceAsWritten().sloc();
                       case delta::Modifier::CPP: return acc + u->sourcePreprocessed().sloc();
                       case delta::Modifier::Cov: return acc + u->sourceWithCoverage().sloc();
                       default: throw std::runtime_error("Unknown modifier");
                     }
                   });
          },
          [](const Units &) -> std::optional<double> { return {}; },
      };
    case delta::Kind::LLOCAbs:
      return {
          [=](const Units &lhs, const Units &) -> double {
            return lhs ^ fold_left(0, [&](auto acc, auto &u) {
                     switch (desc.mod) {
                       case delta::Modifier::Raw: return acc + u->sourceAsWritten().lloc();
                       case delta::Modifier::CPP: return acc + u->sourcePreprocessed().lloc();
                       case delta::Modifier::Cov: return acc + u->sourceWithCoverage().lloc();
                       default: throw std::runtime_error("Unknown modifier");
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
                       default: throw std::runtime_error("Unknown modifier");
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
                   default: throw std::runtime_error("Unknown modifier");
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
          default: throw std::runtime_error("Unknown modifier");
        }
      });
    // semantic cases
    case delta::Kind::STreeRel:
      return treeSelect([=](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: [[fallthrough]];
          case delta::Modifier::CPP: return u->sTree(Unit::View::AsIs);
          case delta::Modifier::Cov: return u->sTree(Unit::View::WithCoverage);
          default: throw std::runtime_error("Unknown modifier");
        }
      });
    case delta::Kind::STreeInlineRel:
      return treeSelect([=](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: [[fallthrough]];
          case delta::Modifier::CPP: return u->sTreeInlined(Unit::View::AsIs);
          case delta::Modifier::Cov: return u->sTreeInlined(Unit::View::WithCoverage);
          default: throw std::runtime_error("Unknown modifier");
        }
      });
    case delta::Kind::IRTreeRel:
      return treeSelect([=](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: [[fallthrough]];
          case delta::Modifier::CPP: return u->irTree(Unit::View::AsIs);
          case delta::Modifier::Cov: return u->irTree(Unit::View::WithCoverage);
          default: throw std::runtime_error("Unknown modifier");
        }
      });
    default: throw std::runtime_error("Unknown kind");
  }
}

int delta::main(int argc, char **argv) {
  using namespace clipp;
  bool help{};
  Options opts{};
  opts.maxThreads = static_cast<int>(std::thread::hardware_concurrency());

  auto pairPattern = [](const std::string &in,
                        char delim) -> std::optional<std::pair<std::string, std::string>> {
    auto pair = in ^ split(delim);
    if (pair.size() == 2) return std::pair{pair[0], pair[1]};
    SV_WARNF("Ignoring malformed pair pattern with delimiter ({}): `{}`", delim, in);
    return std::nullopt;
  };

  auto cli = ( //
      option("--help", "-h").set(help).doc("Show help"),
      repeatable(
          option("--kinds") %
              "Comma separated kinds of metric to use for diff operation. Defaults to all  "
              "supported kinds.\n"
              "Source-based:\n"
              "\tsloc         - Source Lines of Code (Absolute measure).\n"
              "\tlloc         - Logical Lines of Code (Absolute measure).\n"
              "\tsource       - Source code (whitespace normalised edit difference).\n"
              "\ttstree       - Tree-sitter tree.\n"
              "Semantic-based:\n"
              "\tstree        - AST based (e.g ClangAST/High GIMPLE, etc) semantic tree with "
              "\tsymbols normalised.\n"
              "\tstreeinlined - AST based semantic tree with symbols normalised and calls "
              "inlined.\n"
              "\tirtree       - IR based (e.g LLVM IR/Low GIMPLE) semantic tree with symbols "
              "normalised.\n"
              "\nBy default the metric uses the source code or semantic as written, the following "
              "modifiers for each kind are supported:\n"
              "\t+raw - Source code or semantic as written, no transformation\n"
              "\t+cpp - Source after preprocessor has been executed (no-op for semantic kinds)\n"
              "\t+cov - Source after pruning with coverage data if available in the database\n" //
          & value("kind[+modifier],...",
                  [&](const std::string &s) {
                    s ^ split(",") | collect([](auto &spec) -> std::optional<TaskDesc> {
                      auto parts = spec ^ split("+");
                      if (parts.size() == 1) {
                        if (auto k = delta::parseKind(parts[0]); k)
                          return TaskDesc{*k, delta::Modifier::Raw};

                      } else if (parts.size() == 2) {
                        auto k = delta::parseKind(parts[0]);
                        auto m = delta::parseModifier(parts[1]);
                        if (k && m) return TaskDesc{*k, *m};
                      }
                      SV_WARNF("Ignoring malformed kind: {} (parsed as [{}])", spec,
                               parts ^ mk_string(","));
                      return std::nullopt;
                    }) | for_each([&](auto x) { opts.kinds.emplace_back(x); });
                  })),

      option("--output")                                                //
              % "The output file name prefix for all result CSV files." //
          & value("output", opts.outputPrefix),

      option("--threads", "-j")                                                             //
              % "Number of parallel jobs in parallel, defaults to total number of threads." //
          & value("threads", opts.maxThreads),

      repeatable(                                  //
          option("--excludes")                     //
              % "Exclude TUs that match the glob." //
          & value("glob",
                  [&](const std::string &s) {
                    opts.excludes.emplace_back(ExcludeFilter{.glob = s}); //
                  })),

      repeatable(                                                                              //
          option("--merges")                                                                   //
              % "Combine multiple TUs matching the glob into a single TU with the given name." //
          & value("glob:name",
                  [&](const std::string &s) {
                    pairPattern(s, ':') ^ for_each([&](auto &glob, auto &name) {
                      opts.merges.emplace_back(EntryMerge{.glob = glob, .name = name}); //
                    });
                  })),

      repeatable(             //
          option("--matches") //
              % "Glob pairs for matching TUs against the base TUs: diff will run on the first "
                "matching pattern pair. This overrides the default behaviour where TUs are matched "
                "by identical filenames." //
          & value("base glob:model glob",
                  [&](const std::string &s) {
                    pairPattern(s, ':') ^ for_each([&](auto &source, auto &target) {
                      opts.matches.emplace_back(
                          EntryMatch{.sourceGlob = source, .targetGlob = target}); //
                    });
                  })),

      values("[:]database",
             [&](const std::string &s) {
               auto base = s ^ starts_with(":");
               opts.databases.emplace_back(
                   DatabaseSpec{.base = base, .path = base ? s.substr(0, 1) : s});
             }) //
          % "List of database paths to participate in the diff. Prepend the `:` character to "
            "denote a base to compare against. The first positional database will be selected as "
            "base if no paths are annotated as base.");

  auto r = parse(argc, argv, cli);
  if (!r.missing().empty()) {
    SV_ERRF("Missing the following arguments: {}",
            r.missing() ^ mk_string(", ", [](auto e) { return e.param()->label(); }));
  }
  if (!r || help) {
    std::cerr << make_man_page(cli, argv[0]) << std::endl;
    return EXIT_FAILURE;
  }

  return run(opts);
}

int delta::run(const delta::Options &options) {

  SV_COUT //
      << "Build:\n"
      << " - Databases:        \n";
  for (auto &spec : options.databases)
    SV_COUT << "   - " << spec.path << (spec.base ? " (base)" : "") << "\n";
  SV_COUT //
      << " - Kinds:  "
      << (options.kinds ^ //
          mk_string(
              ",",
              [](auto x) { return fmt::format("{}+{}", to_string(x.kind), to_string(x.mod)); }))
      << "\n"
      << " - Excludes:  "
      << (options.excludes ^ //
          mk_string(",", [](auto x) { return x.glob; }))
      << "\n"
      << " - Merges:  "
      << (options.merges ^ //
          mk_string(",", [](auto x) { return x.glob + "->" + x.name; }))
      << "\n"
      << " - Matches:  "
      << (options.matches ^ //
          mk_string(",", [](auto x) { return x.sourceGlob + ":" + x.targetGlob; }))
      << "\n"
      << " - Max threads:  " << options.maxThreads << "\n";

  par_setup(options.maxThreads);

  if (options.databases.empty()) {
    SV_ERRF("At least 1 database required for comparison");
    return EXIT_FAILURE;
  }

  SV_COUT << "# Loading " << options.databases.size() << " models ..." << std::endl;

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

  SV_INFOF("All models loaded");

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

    SV_INFOF("Created {} tasks", deltaTasks.size());

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
      SV_COUT << (row ^ mk_string(", ")) << std::endl;
    }
    std::cout << "\n#delta\n";
    auto d = tabulate(diffs, [](auto v) { return std::to_string(v); });
    for (auto row : d) {
      SV_COUT << (row ^ mk_string(", ")) << std::endl;
    }

    // MEMORY
    // SAVE FORMAT

    //    m.x;

    //              model
    // kind, entry
  }

  return EXIT_SUCCESS;
}
