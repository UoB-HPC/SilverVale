#pragma once

#include "processor_incl_a.h"
#include "processor_incl_b.h"

#define BAZ 1

# pragma foo
#    pragma bar
#pragma baz
#pragma omp  "a"
void baz() {}