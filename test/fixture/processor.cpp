#include "processor.h"
#include "processor_incl_a.h"
#include "processor_incl_sys.h"



#define UNUSED	1	// TABs


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
int main() { return FOO + BAR + BAZ + SYSTEM_HEADER; } // trailing

#define FUNC f1
#include "processor_incl_c.h"
#undef FUNC
#define FUNC f2
#include "processor_incl_c.h"

void end() {}