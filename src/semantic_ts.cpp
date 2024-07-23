#include <iostream>

#include "agv/semantic_ts.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"

using namespace aspartame;

std::pair<std::vector<std::string>, std::unordered_map<std::string, std::string>>
agv::parseCPPLineMarkers(const std::string &iiContent) {
  // see https://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html
  const static std::string prefix = "# ";
  std::unordered_map<std::string, std::string> files;
  std::vector<std::string> encountered;
  std::unordered_set<std::string> witnessed;
  std::string currentFile;

  auto push = [&](auto next) {
    currentFile = next;
    if (auto it = witnessed.emplace(next); it.second) { encountered.emplace_back(currentFile); }
  };

  for (const auto &line : iiContent ^ lines()) {
    if (line.starts_with(prefix)) { // # linenum "filename" flags
      auto marker = line.substr(prefix.length());
      auto validLineNum = marker ^ take_while([](auto c) { return c != ' '; }) ^
                          forall([](auto x) { return std::isdigit(x); });
      if (validLineNum) { // line marker started with a number first, keep going
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
        continue; // eat the line even on parse failure as we are sure it's a line marker
      }
    }
    auto &content = files[currentFile];
    content += line;
    content += "\n";
  }
  return {encountered, files};
}

agv::TsTree::TsTree() = default;
agv::TsTree::TsTree(const std::string &source, const TSLanguage *lang)
    : source(source),
      parser(std::shared_ptr<TSParser>(ts_parser_new(), [](auto x) { ts_parser_delete(x); })) {
  ts_parser_set_language(parser.get(), lang);
  tree = std::shared_ptr<TSTree>(
      ts_parser_parse_string(parser.get(), nullptr, source.c_str(), source.size()),
      [](auto x) { ts_tree_delete(x); });
}

TSNode agv::TsTree::root() const { return ts_tree_root_node(tree.get()); }

static void deleteNodes(const TSNode &node, const std::string &type, size_t &offset,
                        std::string &out);
static void deleteNodes( // NOLINT(*-no-recursion)
    const TSNode &node, const std::string &type, size_t &offset, std::string &out) {
  if (std::string(ts_node_type(node)) == type) {
    auto start = ts_node_start_byte(node);
    auto end = ts_node_end_byte(node);
    out.erase(start - offset, end - start);
    offset += end - start;
  } else {
    for (uint32_t i = 0; i < ts_node_child_count(node); ++i) {
      deleteNodes(ts_node_child(node, i), type, offset, out);
    }
  }
}

agv::TsTree agv::TsTree::deleteNodes(const std::string &type,
                                     const std::optional<TSNode> &node) const {
  size_t offset = 0;
  std::string out = source;
  ::deleteNodes(node.value_or(root()), type, offset, out);
  return {out, ts_parser_language(parser.get())};
}

agv::TsTree agv::TsTree::normaliseNewLines(const std::optional<TSNode> &node) const {
  auto linesToKeep = slocLines(node.value_or(root()));
  auto slocNormalise =
      (source ^ lines())                                                    //
      | zip_with_index<uint32_t>()                                          //
      | filter([&](auto l, auto idx) { return linesToKeep.contains(idx); }) //
      | fold_left(std::string{}, [](auto acc, auto x) { return acc + "\n" + x.first; });
  return {slocNormalise ^ trim(), ts_parser_language(parser.get())};
}

void markWSRanges( // NOLINT(*-no-recursion)
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

agv::TsTree agv::TsTree::normaliseWhitespaces(uint32_t maxWS,
                                              const std::optional<TSNode> &node) const {
  std::string out = source;
  std::vector<std::pair<uint32_t, uint32_t>> ranges;
  markWSRanges(node.value_or(root()), out, maxWS, ranges);
  if (ranges.empty()) { return {out, ts_parser_language(parser.get())}; }

  auto sortedRanges = ranges ^ sort();
  std::vector<std::pair<uint32_t, uint32_t>> merged{sortedRanges[0]};
  for (auto r : sortedRanges) {
    auto &last = merged.back();
    if (r.first <= last.second) last.second = std::max(last.second, r.second);
    else merged.push_back(r);
  }
  for (auto it = sortedRanges.rbegin(); it != sortedRanges.rend(); ++it) {
    out.erase(it->first, it->second - it->first);
  }
  return {out, ts_parser_language(parser.get())};
}

std::set<uint32_t> agv::TsTree::slocLines(const std::optional<TSNode> &node) const {
  std::set<uint32_t> slocLines;
  walk(
      [&](TSNode x) {
        std::string type = ts_node_type(x);

        if (type == "translation_unit" || type == "program") {
          // Some parser's TU would use the end of the file as end point
          // Fortran's TU conditionally contain an extra top-level `program` container if used
          return true;
        }

        if (type.starts_with("preproc_")) {
          // preproc adds a trailing newline, so we just parse what's actually inside
          return true;
        }

        if (type == "end_program_statement" && ts_node_named_child_count(x) == 1) {
          // XXX handle Fortran's trailing newline after `end program $name`
          slocLines.emplace(ts_node_start_point(x).row);
          slocLines.emplace(ts_node_end_point(ts_node_named_child(x, 0)).row);
          return true;
        }

        slocLines.emplace(ts_node_start_point(x).row);
        slocLines.emplace(ts_node_end_point(x).row);
        return true;
      },
      node, false);
  return slocLines;
}

std::set<std::pair<uint32_t, uint32_t>>
agv::TsTree::llocRanges(const std::optional<TSNode> &node) const {
  std::set<std::pair<uint32_t, uint32_t>> llocRanges;
  walk(
      [&](auto x) {
        std::string type = ts_node_type(x);
        // don't count compound stmt as it contains children
        if (type == "compound_statement") return true;

        if (type == "end_program_statement" && ts_node_named_child_count(x) == 1) {
          // XXX handle Fortran's trailing newline after `end program $name`
          llocRanges.emplace(ts_node_start_byte(x), ts_node_end_byte(ts_node_named_child(x, 0)));
          return true;
        }

        if (type ^ ends_with("declaration") || //
            type ^ ends_with("statement") ||   //
            type ^ ends_with("directive")      //
        ) {
          llocRanges.emplace(ts_node_start_byte(x), ts_node_end_byte(x));
        }
        return true;
      },
      node);
  return llocRanges;
}

size_t agv::TsTree::sloc(const std::optional<TSNode> &node) const { //
  return slocLines(node).size();
}
size_t agv::TsTree::lloc(const std::optional<TSNode> &node) const { //
  return llocRanges(node).size();
}
