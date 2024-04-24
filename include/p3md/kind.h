#pragma once
#include <cstdint>
namespace p3md {
enum class DataKind : uint8_t { Source = 1, TSTree, STree, STreeInline };
inline std::string_view to_string(const DataKind &kind) {
  switch (kind) {
    case DataKind::Source: return "source";
    case DataKind::TSTree: return "tstree";
    case DataKind::STree: return "stree";
    case DataKind::STreeInline: return "stree+i";
  }
}
} // namespace p3md