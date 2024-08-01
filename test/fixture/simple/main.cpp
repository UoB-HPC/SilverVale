#include "second.h"
#include <cmath>
#include <string>

#define MACRO 128 + 1

template <typename X> std::string foo() { return "bar" + X{}; }

extern "C" double baz(double x) { return std::fmin(1, x); }

extern "C" double deadcode() { return baz(42); }

#ifdef NOT_DEFINED
// comment should not appear
double guarded() { return deadcode(); }
#endif

int main() {
  std::printf("> %s %f %d %d\n", foo<std::string>().c_str(), baz(43), bar(), MACRO);

  if (baz(-1) < 0) {     // comment should not appear
    std::printf("OK\n"); // comment should not appear
  } else {               // comment should not appear
    std::printf("FAIL\n");
  }

#ifdef NOT_DEFINED
  std::printf("nope %f\n", guarded());
#endif
  return EXIT_SUCCESS;
}
