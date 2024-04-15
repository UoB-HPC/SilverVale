#pragma once
#include <cstdint>
namespace p3md {
enum class DataKind : uint8_t { Source = 1, TSTree, STree, STreeInline };
} // namespace p3md