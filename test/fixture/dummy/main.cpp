#include <cmath>
#include <string>
#include "second.h"

std::string foo() { return "bar"; }
extern "C" double baz() { return std::fmin(1, 2.0); }

int main() {
  std::printf("%s %f %d\n", foo().c_str(), baz(), bar());
  return EXIT_SUCCESS;
}
