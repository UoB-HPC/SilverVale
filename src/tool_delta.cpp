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

template <typename T> class CountingSemaphore {
  T maxCount, available;
  std::mutex mutex;
  std::condition_variable cv;

public:
  explicit CountingSemaphore(T max) : maxCount(max), available(max) {}

  void acquire(T n) {
    std::unique_lock lock(mutex);
    cv.wait(lock, [this, n]() { return available >= n; });
    available -= n;
  }

  void release(T n) {
    std::unique_lock lock(mutex);
    available += n;
    if (available > maxCount) { available = maxCount; }
    cv.notify_all();
  }

  bool raiseAndAcquire(T n) {
    std::unique_lock lock(mutex);
    bool raised = false;
    if (n > maxCount) {
      available += (n - maxCount);
      maxCount = n;
      raised = true;
    }
    cv.wait(lock, [this, n]() { return available >= n; });
    available -= n;
    return raised;
  }

  void reset(T max) {
    std::unique_lock lock(mutex);
    maxCount = max;
    available = max;
    cv.notify_all();
  }
};

static CountingSemaphore<size_t> streeTokens{0};

template <typename F> std::pair<DiffFn, MaxFn> treeDiff(delta::TaskDesc desc, F f) {
  return {
      [=](const Units &lhs, const Units &rhs) -> double {
        const Tree lt = Tree::combine("root", lhs ^ map([&](auto &x) { return f(x); }));
        const Tree rt = Tree::combine("root", lhs ^ map([&](auto &x) { return f(x); }));
        const auto requiredNodes = lt.nodes() * rt.nodes();
        if (streeTokens.raiseAndAcquire(requiredNodes)) {
          SV_WARNF("\nRaising concurrent node limit to {} to avoid deadlock for task {}+{} "
                   "([{}]={} v.s [{}]={})",
                   requiredNodes, delta::to_string(desc.kind), delta::to_string(desc.mod),
                   lhs ^ mk_string(",", [](auto &x) { return x->name(); }), lt.nodes(),
                   rhs ^ mk_string(",", [](auto &x) { return x->name(); }), rt.nodes());
        }
        auto result = Diff::apted(lt, rt);
        streeTokens.release(requiredNodes);
        return result;
      },
      [=](const Units &xs) -> std::optional<double> {
        return Tree::combine("root", xs ^ map([&](auto &x) { return f(x); })).nodes();
      },
  };
}

static double minDiff(std::vector<std::string> &ls, std::vector<std::string> &rs) {
  auto value = std::numeric_limits<double>::max();
  do {
    auto lhsSrc = ls ^ mk_string();
    do {
      auto rhsSrc = rs ^ mk_string();
      value = std::fmin(value, static_cast<double>(Diff::diff(lhsSrc, rhsSrc)));
    } while (std::next_permutation(rs.begin(), rs.end()));
  } while (std::next_permutation(ls.begin(), ls.end()));
  return value;
}

static std::pair<DiffFn, MaxFn> createTask(delta::TaskDesc desc) {
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
                       default: throw std::runtime_error("Unknown modifier");
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
                         return acc + (u->sourceAsWritten().contentWhitespaceNormalised() ^
                                       fold_left(0, [](auto acc, auto &s) { return s.size(); }));
                       case delta::Modifier::CPP:
                         return acc + (u->sourcePreprocessed().contentWhitespaceNormalised() ^
                                       fold_left(0, [](auto acc, auto &s) { return s.size(); }));
                       case delta::Modifier::Cov:
                         return acc + (u->sourceWithCoverage().contentWhitespaceNormalised() ^
                                       fold_left(0, [](auto acc, auto &s) { return s.size(); }));
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
                     return x->sourceAsWritten().contentWhitespaceNormalised() ^ mk_string();
                   case delta::Modifier::CPP:
                     return x->sourcePreprocessed().contentWhitespaceNormalised() ^ mk_string();
                   case delta::Modifier::Cov:
                     return x->sourceWithCoverage().contentWhitespaceNormalised() ^ mk_string();
                   default: throw std::runtime_error("Unknown modifier");
                 }
               });
      };
      return {
          [=](const Units &lhs, const Units &rhs) -> double {
            auto ls = extract(desc.mod, lhs);
            auto rs = extract(desc.mod, rhs);
            return minDiff(ls, rs);
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
      return treeDiff(desc, [desc](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: return u->sourceAsWritten().tsTree();
          case delta::Modifier::CPP: return u->sourcePreprocessed().tsTree();
          case delta::Modifier::Cov: return u->sourceWithCoverage().tsTree();
          default: throw std::runtime_error("Unknown modifier");
        }
      });
    // semantic cases
    case delta::Kind::STreeRel:
      return treeDiff(desc, [desc](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: [[fallthrough]];
          case delta::Modifier::CPP: return u->sTree(Unit::View::Self);
          case delta::Modifier::Cov: return u->sTree(Unit::View::WithCov);
          default: throw std::runtime_error("Unknown modifier");
        }
      });
    case delta::Kind::STreeInlineRel:
      return treeDiff(desc, [desc](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: [[fallthrough]];
          case delta::Modifier::CPP: return u->sTreeInlined(Unit::View::Self);
          case delta::Modifier::Cov: return u->sTreeInlined(Unit::View::WithCov);
          default: throw std::runtime_error("Unknown modifier");
        }
      });
    case delta::Kind::IRTreeRel:
      return treeDiff(desc, [desc](auto &u) {
        switch (desc.mod) {
          case delta::Modifier::Raw: [[fallthrough]];
          case delta::Modifier::CPP: return u->irTree(Unit::View::Self);
          case delta::Modifier::Cov: return u->irTree(Unit::View::WithCov);
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
  opts.maxSTreeThreads = opts.maxThreads / 2;
  opts.memThrottleRatio = 0.95;
  opts.memRetryIntervalMs = 100;
  opts.maxSTreeTokens = 150000000;

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

      option("--prefix")                                                //
              % "The output file name prefix for all result CSV files." //
          & value("prefix", opts.outputPrefix),

      option("--threads", "-j")                                                             //
              % "Number of parallel jobs in parallel, defaults to total number of threads." //
          & value("threads", opts.maxThreads),

      option("--memRatio") //
              % ("Ratio (0 to 1.0) of memory used before tasks are throttled, defaults to " +
                 std::to_string(opts.memThrottleRatio)) //
          & value("ratio", opts.memThrottleRatio),

      option("--memInterval") //
              % ("Milliseconds to wait before checking for memory pressure, defaults to " +
                 std::to_string(opts.memRetryIntervalMs)) //
          & value("ratio", opts.memRetryIntervalMs),

      option("--maxStreeThreads") //
              % ("Maximum amount of memory intensive stree task to run in parallel, defaults to " +
                 std::to_string(opts.maxSTreeThreads)) //
          & value("ratio", opts.maxSTreeThreads),

      option("--maxGroups") //
              % ("Maximum amount of models to load at once: low values reduce memory usage at the "
                 "cost of CPU utilisation. Defaults to" +
                 std::to_string(opts.maxGroups)) //
          & value("ratio", opts.maxGroups),

      option("--maxSTreeTokens") //
              % ("Maximum amount of total nodes for stree TED tasks at any given time to prevent "
                 "memory exhaustion. Defaults to" +
                 std::to_string(opts.maxSTreeTokens)) //
          & value("ratio", opts.maxSTreeTokens),

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

      values("[@]database:[glob,...]",
             [&](const std::string &s) {
               auto base = s ^ starts_with("@");
               auto spec = base ? s.substr(0, 1) : s;

               pairPattern(spec, ':') ^ for_each([&](auto &path, auto &roots) {
                 opts.databases.emplace_back(DatabaseSpec{              //
                                                          .base = base, //
                                                          .path = path,
                                                          .roots = roots ^ split(",")});
               });
             }) //
          % "List of database paths to participate in the diff. Prepend the `@` character to "
            "denote a base to compare against. "
            "The first positional database will be selected as  base if no paths are annotated as "
            "base. "
            "An optional CSV list of globs can be added after a colon, dependencies not matching "
            "the pattern will not be analysed.");

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

struct Model {
  std::string path;
  std::vector<std::pair<std::string, Units>> entries;
};

static Model loadModelFromSpec(const delta::Options &options, const delta::DatabaseSpec &spec) {
  const auto excludes = options.excludes ^ map([](auto &f) { return globToRegex(f.glob); });

  const Database db = Codebase::loadDB(spec.path);
  const Codebase cb = Codebase::load(
      db, true, spec.roots, //
      [&](auto &path) {
        return excludes ^ forall([&](auto &r) { return !std::regex_match(path, r); });
      });

  const auto merges =
      options.merges ^ map([](auto &m) { return std::pair{globToRegex(m.glob), m.name}; });
  Model model{.path = cb.root,
              .entries =                //
              cb.units                  //
              ^ group_by([&](auto &u) { //
                  return (merges        //
                          |
                          filter([&](auto &r, auto &) { return std::regex_match(u->path(), r); }) //
                          | values()                                                              //
                          | head_maybe()) ^                                                       //
                         get_or_else(u->name());                                                  //
                })                                                                                //
              ^ to_vector() ^ sort_by([](auto &name, auto &us) {                                  //
                  return std::pair{name,                                                          //
                                   us ^ fold_left(0, [](auto acc, auto &u) {
                                     return acc + u->sourceAsWritten().sloc();
                                   })};
                })};
  SV_INFOF("[[ {} ]]", model.path);
  for (auto &[name, us] : model.entries) {
    SV_INFOF("  {} -> [{}]", name,
             (us ^ mk_string(", ", [](auto &u) { return fmt::format("{}", u->path()); })));
  }
  return model;
}

struct Key {
  delta::TaskDesc desc{};
  std::string name{};
  size_t modelIdx{};
};

using DiffTask =
    std::pair<Key, std::variant<std::tuple<DiffFn, Units, Units>, std::tuple<MaxFn, Units>>>;

using Result = std::pair<Key, std::variant<double, std::optional<double>>>;

static void generateTasks(const delta::Options &options, const Model &base,
                          const std::vector<std::pair<delta::DatabaseSpec, size_t>> &specs,
                          std::vector<Result> &results) {
  std::vector<DiffTask> deltaTasks;

  auto models = specs ^ map([&](auto &spec, auto idx) {
                  return std::pair{loadModelFromSpec(options, spec), idx};
                });

  options.kinds                                                           //
      | map([](auto &desc) { return std::pair{desc, createTask(desc)}; }) //
      | for_each([&](auto &desc, auto &p) {
          auto [delta, max] = p;
          base.entries | for_each([&](auto &lhsName, const Units &l) {
            auto addTask = [&](size_t idx, auto fn) {
              deltaTasks.emplace_back(
                  std::pair{Key{.desc = desc, .name = lhsName, .modelIdx = idx}, fn});
            };
            models | for_each([&](auto &r, size_t idx) { //
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
             return acc + u->sourceAsWritten().content().size();
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

  auto [streeTasks, otherTasks] = taskReverseSizes ^ partition([](auto &key, auto) {
                                    switch (key.desc.kind) {
                                      case sv::delta::Kind::STreeRel: [[fallthrough]];
                                      case sv::delta::Kind::STreeInlineRel: [[fallthrough]];
                                      case sv::delta::Kind::TSTreeRel: [[fallthrough]];
                                      case sv::delta::Kind::IRTreeRel: return true;
                                      default: return false;
                                    }
                                  });

  auto logger = ProgressLogger{deltaTasks.size(),
                               deltaTasks ^ fold_left(int{}, [](auto acc, auto &t) {
                                 return std::max(acc, static_cast<int>(t.first.name.size()));
                               })};

  Cached<MemInfo> memInfo(std::chrono::milliseconds(options.memRetryIntervalMs));

  auto runTasks = [&](const std::string &name, const std::vector<DiffTask> &tasks,
                      size_t maxThreads) {
    SV_INFOF("Executing group <{}> of {} tasks (maxThreads={}, memRatio={} memInterval={}ms)", name,
             tasks.size(), maxThreads, options.memThrottleRatio, options.memRetryIntervalMs);
    auto start = std::chrono::high_resolution_clock::now();
    auto completed = par_map(maxThreads, tasks, [&memInfo, &options, &logger](auto &task) {
      auto usedPct = memInfo(&MemInfo::read).ramUsedPct();
      while (usedPct > options.memThrottleRatio) {
        std::this_thread::sleep_for(std::chrono::milliseconds(options.memRetryIntervalMs));
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
    auto end = std::chrono::high_resolution_clock::now();
    SV_INFOF("Completed {} tasks in {}", tasks.size(),
             std::chrono::duration_cast<std::chrono::seconds>(end - start));
    results.insert(results.end(), completed.begin(), completed.end());
  };
  runTasks("stree", streeTasks, options.maxSTreeThreads);
  runTasks("other", otherTasks, options.maxThreads);
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

  streeTokens.reset(options.maxSTreeTokens);

  if (options.databases.empty()) {
    SV_ERRF("At least 1 database required for comparison");
    return EXIT_FAILURE;
  }

  auto base = loadModelFromSpec(options, options.databases[0]);
  std::vector<Result> allResults;
  auto groups = (options.databases | zip_with_index() | to_vector()) ^ grouped(options.maxGroups) ^
                zip_with_index();

  SV_INFOF("Loading {} models together, totaling for {} batches for {} models", options.maxGroups,
           groups.size(), options.databases.size());
  auto totalStart = std::chrono::high_resolution_clock::now();
  for (auto &[xs, group] : groups) {
    auto start = std::chrono::high_resolution_clock::now();
    SV_INFOF("Submitting group {}: {}", //
             group, xs | mk_string(", ", [](auto &spec, auto idx) {
                      return fmt::format("[{}]({})", idx, spec.path);
                    }));
    generateTasks(options, base, xs, allResults);
    auto end = std::chrono::high_resolution_clock::now();
    SV_INFOF("Group {} completed in {}", //
             group, std::chrono::duration_cast<std::chrono::seconds>(end - start));
  }
  auto totalEnd = std::chrono::high_resolution_clock::now();
  SV_INFOF("All tasks completed in {}",
           std::chrono::duration_cast<std::chrono::seconds>(totalEnd - totalStart));

  auto modelPaths = options.databases ^ map([](auto s) { return s.path.string(); });
  auto tabulate = [&]<typename T>(const std::vector<std::pair<Key, T>> &xs, auto f) {
    size_t prefixLen = longestCommonPrefixLen(modelPaths);
    return (xs ^ group_map([](auto &k, auto) { return k.modelIdx; },                       //
                           [](auto &k, auto v) { return std::tuple{k.desc, k.name, v}; })) //
           ^ to_vector()                                                                   //
           ^ sort_by([](auto &modelIdx, auto) { return modelIdx; })                        //
           ^ bind([&](auto &modelIdx, auto &xs) {                                          //
               auto modelName = modelPaths[modelIdx].substr(prefixLen);
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

  auto maxes = //
      allResults ^ collect([](auto &k, auto &v) {
        return v ^ get<std::optional<double>>() ^ map([&](auto v) { return std::pair{k, v}; });
      });
  auto diffs //
      = allResults ^ collect([](auto &k, auto &v) {
          return v ^ get<double>() ^ map([&](auto v) { return std::pair{k, v}; });
        });

  auto totals = [](auto diffs, auto f) {
    return diffs                                                                                //
           | group_map([](auto &k, auto &v) { return k.modelIdx; },                             //
                       [](auto &k, auto &v) { return std::pair{k.desc, v}; })                   //
           | bind([&](auto &i, auto &xs) {                                                      //
               return xs                                                                        //
                      | group_map_reduce([](auto &x, auto) { return x; },                       //
                                         [&](auto &, auto &v) { return f(v); }, std::plus<>())  //
                      | map([&](auto &k, auto &v) { return std::pair{Key{k, "total", i}, v}; }) //
                      | to_vector();                                                            //
             })                                                                                 //
           | to_vector();
  };

  {
    std::ofstream out(fmt::format("{}.model_max.csv", options.outputPrefix));
    for (auto &row : tabulate(maxes, [](auto v) { return fmt::format("{}", v.value_or(-1)); })) {
      out << (row ^ mk_string(",")) << "\n";
    }
  }

  {
    std::ofstream out(fmt::format("{}.model_max.total.csv", options.outputPrefix));
    for (auto &row : tabulate(totals(maxes, [](auto m) { return m.value_or(0); }),
                              [](auto v) { return fmt::format("{}", v); })) {
      out << (row ^ mk_string(",")) << "\n";
    }
  }

  {
    std::ofstream out(fmt::format("{}.model_diffs.csv", options.outputPrefix));
    for (auto &row : tabulate(diffs, [](auto v) { return fmt::format("{}", v); })) {
      out << (row ^ mk_string(",")) << "\n";
    }
  }

  {
    std::ofstream out(fmt::format("{}.model_diffs.total.csv", options.outputPrefix));
    for (auto &row :
         tabulate(totals(diffs, std::identity{}), [](auto v) { return fmt::format("{}", v); })) {
      out << (row ^ mk_string(",")) << "\n";
    }
  }

  //    std::cout << "\n#max\n";
  //    auto m = tabulate(maxes, [](auto v) { return fmt::format("{}", v.value_or(-1)); });
  //    for (auto row : m) {
  //      SV_COUT << (row ^ mk_string(", ")) << std::endl;
  //    }
  //    std::cout << "\n#delta\n";
  //    auto d = tabulate(diffs, [](auto v) { return fmt::format("{}", v); });
  //    for (auto row : d) {
  //      SV_COUT << (row ^ mk_string(", ")) << std::endl;
  //    }

  // MEMORY
  // SAVE FORMAT

  //    m.x;

  //              model
  // kind, entry

  return EXIT_SUCCESS;
}
