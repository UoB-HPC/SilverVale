#include <iostream>

#include "sv/tree_ts.h"

#include "aspartame/string.hpp"
#include "aspartame/unordered_map.hpp"
#include "aspartame/vector.hpp"

using namespace aspartame;

std::pair<std::vector<std::string>, std::unordered_map<std::string, std::string>>
sv::parseCPPLineMarkers(const std::string &iiContent) {
  // see https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
  const static std::string prefix = "# ";
  std::unordered_map<std::string, std::pair<size_t, std::string>> files;
  std::vector<std::string> encountered;
  std::unordered_set<std::string> witnessed;
  std::string currentFile;
  size_t currentLine;

  auto push = [&](auto next) {
    currentFile = next;
    if (auto it = witnessed.emplace(next); it.second) { encountered.emplace_back(currentFile); }
  };

  for (const auto &line : iiContent ^ lines()) {
    if (line.starts_with(prefix)) { // # linenum "filename" flags
      auto marker = line.substr(prefix.length());
      std::optional<size_t> lineNum;
      try {
        lineNum = std::stoul(marker ^ take_while([](auto c) { return c != ' '; }));
      } catch (...) {}
      if (lineNum) { // line marker started with a number first, keep going
        // the path may be quoted with spaces in between, so match up the quotes
        auto quoteIndices = marker                                                    //
                            | zip_with_index()                                        //
                            | collect([](auto c, auto idx) -> std::optional<size_t> { //
                                return c == '"' ? std::optional{idx} : std::nullopt;  //
                              })                                                      //
                            | to_vector();
        if (quoteIndices.empty()) { // not quoted: `linenum filename flags`
          auto fragments = marker ^ split(' ');
          if (fragments.size() >= 2) push(fragments[1]);
        } else if (quoteIndices.size() > 1) { // quoted: `linenum "filename" flags`
          auto start = quoteIndices.front() + 1;
          auto len = quoteIndices.back() - start;
          push(marker.substr(start, len)); // pop the quotes
        }
        currentLine = *lineNum - 1;
        continue; // eat the line even on parse failure as we are sure it's a line marker
      }
    }
    auto &[lines, content] = files[currentFile];
    if (lines < currentLine) {
      content.append(currentLine - lines, '\n');
      lines = currentLine;
    }
    content += line;
    content += '\n';
    lines++;
  }
  return {encountered, files ^ map_values([](auto, auto l) { return l; })};
}

sv::TsTree::TsTree() = default;
sv::TsTree::TsTree(const std::string &name, const std::string &source, const TSLanguage *lang)
    : name(std::filesystem::path(name).filename()), source(source),
      parser(std::shared_ptr<TSParser>(ts_parser_new(), [](auto x) { ts_parser_delete(x); })) {
  ts_parser_set_language(parser.get(), lang);
  tree = std::shared_ptr<TSTree>(
      ts_parser_parse_string(parser.get(), nullptr, source.c_str(), source.size()),
      [](auto x) { ts_tree_delete(x); });
}

TSNode sv::TsTree::root() const { return ts_tree_root_node(tree.get()); }

static void without( // NOLINT(*-no-recursion)
    const TSNode &node, const std::string &type, size_t &offset, std::string &out) {
  if (std::string(ts_node_type(node)) == type) {
    auto start = ts_node_start_byte(node);
    auto end = ts_node_end_byte(node);
    out.erase(start - offset, end - start);
    offset += end - start;
  } else {
    for (uint32_t i = 0; i < ts_node_child_count(node); ++i) {
      without(ts_node_child(node, i), type, offset, out);
    }
  }
}

sv::TsTree sv::TsTree::without(const std::string &type, const std::optional<TSNode> &node) const {
  size_t offset = 0;
  std::string out = source;
  ::without(node.value_or(root()), type, offset, out);
  return {name, out, ts_parser_language(parser.get())};
}

sv::TsTree sv::TsTree::normaliseNewLines(const std::optional<TSNode> &node) const {
  auto linesToKeep = slocLines({}, node.value_or(root()));
  auto slocNormalise = (source ^ lines())                                                    //
                       | zip_with_index<uint32_t>(1)                                          //
                       | filter([&](auto l, auto idx) { return linesToKeep.contains(idx); }) //
                       | keys()                                                              //
                       | mk_string("\n");                                                    //
  return {name, slocNormalise ^ trim(), ts_parser_language(parser.get())};
}

static void markWSRanges( // NOLINT(*-no-recursion)
    const TSNode &root, const std::string &content, uint32_t maxWs,
    std::vector<std::pair<uint32_t, uint32_t>> &xs) {
  auto count = ts_node_child_count(root);
  for (uint32_t i = 0; i < count; ++i) {
    auto node = ts_node_child(root, i);
    if (i + 1 < count) {
      auto next = ts_node_child(root, i + 1);
      auto start = ts_node_end_byte(node), end = ts_node_start_byte(next);
      if (end - start > maxWs) {
        if (auto nlIdx = content.substr(start, end - start) ^ index_of('\n'); nlIdx != -1) {
          // delete anything before and after N*NL, but not the NL itself
          // Example with @=' ': `int a;@@\n@@@//` becomes `int a'@\n//`
          xs.emplace_back(start, start + nlIdx);
          xs.emplace_back(start + nlIdx + maxWs, end);
        } else {
          xs.emplace_back(start + maxWs, end); // keep N*WS
        }
      }
    }
    markWSRanges(node, content, maxWs, xs);
  }
}

// TODO FIXME breaks #define in Fortran
sv::TsTree sv::TsTree::normaliseWhitespaces(uint32_t maxWS,
                                            const std::optional<TSNode> &node) const {
  std::vector<std::pair<uint32_t, uint32_t>> ranges;
  markWSRanges(node.value_or(root()), source, maxWS, ranges);
  return deleteRanges(ranges);
}

std::set<uint32_t> sv::TsTree::slocLines(const std::function<bool(const TSNode &)> &mask,
                                         const std::optional<TSNode> &node) const {
  std::set<uint32_t> slocLines;
  preOrderWalk(
      [&](TSNode x) {
        std::string type = ts_node_type(x);
        if (mask && mask(x)) { return true; }

        if (type == "translation_unit" || type == "program") {
          // Some parser's TU would use the end of the file as end point
          // Fortran's TU conditionally contain an extra top-level `program` container if used
          return true;
        }

        if (type.starts_with("preproc_")) {
          // preproc adds a *trailing* newline, so we just parse what's actually inside
          slocLines.emplace(ts_node_start_point(x).row +
                            1); // trailing, so the start is still correct
          return true;
        }

        if (type == "end_program_statement" && ts_node_named_child_count(x) == 1) {
          // XXX handle Fortran's trailing newline after `end program $name`
          slocLines.emplace(ts_node_start_point(x).row + 1);
          slocLines.emplace(ts_node_end_point(ts_node_named_child(x, 0)).row + 1);
          return true;
        }

        slocLines.emplace(ts_node_start_point(x).row + 1);
        slocLines.emplace(ts_node_end_point(x).row + 1);
        return true;
      },
      node, false);
  return slocLines;
}

std::set<std::pair<uint32_t, uint32_t>>
sv::TsTree::llocRanges(const std::function<bool(const TSNode &)> &mask,
                       const std::optional<TSNode> &node) const {
  std::set<std::pair<uint32_t, uint32_t>> llocRanges;
  preOrderWalk(
      [&](auto &x) {
        if (mask && mask(x)) { return true; }

        std::string type = ts_node_type(x);
        // don't count compound stmt as it contains children
        if (type == "compound_statement") return true;

        if (type == "end_program_statement" && ts_node_named_child_count(x) == 1) {
          // XXX handle Fortran's trailing newline after `end program $name`
          llocRanges.emplace(ts_node_start_byte(x), ts_node_end_byte(ts_node_named_child(x, 0)));
          return true;
        }

        if (type ^ ends_with("directive")) {
          // preproc_directive is enclosed by a preproc_call which is the logical part,
          // but it may contain excessive new lines, so we take the min and max of the children
          const auto parent = ts_node_parent(x);
          auto start = ts_node_start_byte(ts_node_child(parent, 0));
          auto end = ts_node_end_byte(ts_node_child(parent, ts_node_child_count(parent) - 1));
          llocRanges.emplace(start, end);
          return true;
        }

        if (type ^ ends_with("declaration") || //
            type ^ ends_with("statement")      //
        ) {
          llocRanges.emplace(ts_node_start_byte(x), ts_node_end_byte(x));
        }
        return true;
      },
      node, true);
  return llocRanges;
}

sv::TsTree
sv::TsTree::deleteRanges(const std::vector<std::pair<uint32_t, uint32_t>> &ranges) const {
  if (ranges.empty()) { return *this; }
  std::string out = source;
  auto sortedRanges = ranges ^ sort_by([](auto x, auto) { return x; });
  std::vector<std::pair<uint32_t, uint32_t>> merged{sortedRanges[0]};
  for (auto r : sortedRanges) {
    auto &last = merged.back();
    if (r.first <= last.second) last.second = std::max(last.second, r.second);
    else merged.push_back(r);
  }
  for (auto it = merged.rbegin(); it != merged.rend(); ++it) {
    out.erase(it->first, it->second - it->first);
  }
  return {name, out, ts_parser_language(parser.get())};
}
