#pragma once
#include <cstdint>
namespace p3md {
enum class DataKind : uint8_t { SLOC = 1, LLOC, Source, TSTree, STree, STreeInline, IRTree };

// p3md.par({ name:string, task:()=>() })

// Database
//   load(normalise: bool, string => bool): Codebase

// Codebase
//   path: String
//   units: Unit
// Unit
//   name(): String
//   path(): String
//   source(): Source
//   stree(): Tree
//   stree+i(): Tree
//   irtree(): Tree
//   ::merge(name:string, xs...): Unit
// Source
//   content(): String
//   tstree(): Tree
//   sloc(): int
//   lloc(): int
// Tree
//   nodes() int
//   maxDepth(): Int
//   maxWidth(): Int

// ted: (Tree, Tree) => Int
// diff: (String, String) => Int

// local libs = {..}
// local dbs = libs.par(Database.froomJson(_))

// Database.loadAll(paths: {string}): {Database}
// Codebase.loadAll(dbs: {Database}, filter: function(string): bool): {Codebase}

// pairs: { string, {string, {unit}} }
// limit: function({unit})

// DiffTask
//   name: string
//   left: {Unit}
//   right: {Unit}
//   fs: { function({Unit},{Unit}): {string, number} }
// DiffRow
//   name: string
//   val: {string: number}

// Codebase.run(tasks: {DiffTask})

//, group : String => string
// filter : String => Bool
// group : String => string
// diff : (Unit[], Unit[]) => Map<string, double>
// max : (Unit[]) => Map<string, double>

inline std::string_view to_string(const DataKind &kind) {
  switch (kind) {
    case DataKind::SLOC: return "sloc";
    case DataKind::LLOC: return "lloc";
    case DataKind::Source: return "source";
    case DataKind::TSTree: return "tstree";
    case DataKind::STree: return "stree";
    case DataKind::STreeInline: return "stree+i";
    case DataKind::IRTree: return "irtree";
  }
}
} // namespace p3md