#include "consume.h"
#include <cstdio>

void consume(void *ptr) {
  if (ptr) { std::printf("Consumed %p\n", ptr); }
}