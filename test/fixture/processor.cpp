#include "processor.h"
#include "processor_incl_a.h"

#ifndef FOO
  #error ""
#endif

#ifndef BAR
  #error ""
#endif

#ifndef BAZ
  #error ""
#endif
//main
int main() { return FOO + BAR + BAZ; }

#define FUNC f1
#include "processor_incl_c.h"
#define FUNC f2
#include "processor_incl_c.h"

void end() {}