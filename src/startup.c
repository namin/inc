#include <stdio.h>
#include "startup.h"
#include "scheme_entry.h"

static void print_ptr(ptr x) {
  if ((x & fx_mask) == fx_tag) {
    printf("%d", ((int) x) >> fx_shift);
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (x == list_nil) {
    printf("()");
  } else if ((x & char_mask) == char_tag) {
    char c = (char) (x >> char_shift);
    if      (c == '\t') printf("#\\tab");
    else if (c == '\n') printf("#\\newline");
    else if (c == '\r') printf("#\\return");
    else if (c == ' ')  printf("#\\space");
    else                printf("#\\%c", c);
  } else {
    printf("#<unknown 0x%08x>", x);
  }
  printf("\n");
}

int main(int argc, char** argv) {
  print_ptr(scheme_entry());
  return 0;
}
