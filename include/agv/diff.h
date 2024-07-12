#pragma once

#include <string>

#include "database.h"
#include "lua.h"

namespace agv {
struct Diff {
  static double apted(const agv::Tree &lhsTree, const agv::Tree &rhsTree);
  static double diff(const std::string &lhs, const std::string &rhs);

  DEF_TEAL_SOL_UT(Diff, SOL_UT_FN(Diff, apted), SOL_UT_FN(Diff, diff));
};
} // namespace agv

