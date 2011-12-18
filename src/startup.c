#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#include "startup.h"
#include "scheme_entry.h"

#define IN_LIST 1
#define OUT_LIST 0

static void print_ptr_rec(ptr x, int state) {
  if ((x & fx_mask) == fx_tag) {
    printf("%ld", ((long) x) >> fx_shift);
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
  } else if ((x & obj_mask) == pair_tag) {
    if (state != IN_LIST) printf("(");
    ptr car = ((cell*)(x-pair_tag))->car;
    print_ptr_rec(car, OUT_LIST);
    ptr cdr = ((cell*)(x-pair_tag))->cdr;
    if (cdr != list_nil) {
      if ((cdr & obj_mask) != pair_tag) {
        printf(" . ");
        print_ptr_rec(cdr, OUT_LIST);
      } else {
        printf(" ");
        print_ptr_rec(cdr, IN_LIST);
      }
    }
    if (state != IN_LIST) printf(")");
  } else {
    printf("#<unknown 0x%08lx>", x);
  }
}

static void print_ptr(ptr x) {
  print_ptr_rec(x, OUT_LIST);
  printf("\n");
}

static char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if (p == MAP_FAILED) { perror("map"); exit(1); }
  status = mprotect(p, page, PROT_NONE);
  if (status != 0) { perror("mprotect"); exit(status); }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if (status != 0) { perror("mprotect"); exit(status); }
  return (p + page);
}

static void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if (status != 0) { perror("munmap"); exit(status); }
}

int main(int argc, char** argv) {
  int stack_size = (16 * 4096); /* holds 16K cells */
  int heap_size = (16 * 4096); /* holds 16K cells */

  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;

  char* heap = allocate_protected_space(heap_size);

  context ctxt;
  print_ptr(scheme_entry(&ctxt, stack_base, heap));

  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap, stack_size);
  return 0;
}
