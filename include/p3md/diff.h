#pragma once

#include "database.h"

namespace p3md {
struct Diff {
  static double apted(const p3md::Tree &lhsTree, const p3md::Tree &rhsTree);
  static double diff(const std::string &lhs, const std::string &rhs);

  USERTYPE_DEFINE(Diff,                     //
                  USERTYPE_FN(Diff, apted), //
                  USERTYPE_FN(Diff, diff));
};
} // namespace p3md
