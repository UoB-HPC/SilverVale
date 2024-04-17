#pragma once

#ifdef __cpp_lib_syncbuf
  #include <syncstream>
  #define P3MD_COUT std::osyncstream(std::cout)
#else
  #include <iostream>
  #define P3MD_COUT std::cout
#endif
