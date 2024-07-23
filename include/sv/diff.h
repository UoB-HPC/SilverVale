#pragma once

#include <string>

#include "lua.h"
#include "model.h"

namespace sv {
struct Diff {
  static double apted(const sv::Tree &lhsTree, const sv::Tree &rhsTree);
  static double diff(const std::string &lhs, const std::string &rhs);

  DEF_TEAL_SOL_UT(Diff, SOL_UT_FN(Diff, apted), SOL_UT_FN(Diff, diff));
};
} // namespace sv
