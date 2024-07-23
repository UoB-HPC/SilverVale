#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include <unistd.h>
#include <vector>

// TODO WIP
int main(int argc, char *argv[]) {

  auto prog = std::getenv("AGV_COMPILER");
  if (!prog) {
    std::cerr << "Wrapped compiler AGV_COMPILER not set" << std::endl;
    return EXIT_FAILURE;
  }

  // gcc
  //  -###  then count /cc1* or f951 invocations, then do set difference to identify files,
  //   => one file at a time or -MD for dependency
  // Clang
  //  -ccc-print-bindings
  //   => one file at a time or -MD for dependency

  std::vector<char *> args{prog};
  for (int i = 1; i < argc; ++i)
    args.push_back(argv[i]);
  args.push_back(nullptr);

  execvp(args[0], args.data());

  std::cerr << "execvp failed: " << strerror(errno) << std::endl;
  return EXIT_FAILURE;
}