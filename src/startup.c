#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#include "startup.h"
#include "scheme_entry.h"

#define IN 1
#define OUT 0

static void print_ptr_rec(FILE* port, ptr x, int state) {
  if ((x & fx_mask) == fx_tag) {
    fprintf(port, "%ld", ((long) x) >> fx_shift);
  } else if (x == bool_f) {
    fprintf(port, "#f");
  } else if (x == bool_t) {
    fprintf(port, "#t");
  } else if (x == list_nil) {
    fprintf(port, "()");
  } else if ((x & char_mask) == char_tag) {
    char c = (char) (x >> char_shift);
    if      (c == '\t') fprintf(port, "#\\tab");
    else if (c == '\n') fprintf(port, "#\\newline");
    else if (c == '\r') fprintf(port, "#\\return");
    else if (c == ' ')  fprintf(port, "#\\space");
    else                fprintf(port, "#\\%c", c);
  } else if ((x & obj_mask) == pair_tag) {
    if (state != IN) fprintf(port, "(");
    ptr car = ((cell*)(x-pair_tag))->car;
    print_ptr_rec(port, car, OUT);
    ptr cdr = ((cell*)(x-pair_tag))->cdr;
    if (cdr != list_nil) {
      if ((cdr & obj_mask) != pair_tag) {
        fprintf(port, " . ");
        print_ptr_rec(port, cdr, OUT);
      } else {
        fprintf(port, " ");
        print_ptr_rec(port, cdr, IN);
      }
    }
    if (state != IN) fprintf(port, ")");
  } else if ((x & obj_mask) == vector_tag) {
    fprintf(port, "#(");

    vector* p = (vector*)(x-vector_tag);
    unsigned long n = p->length >> fx_shift;
    unsigned long i;
    for (i = 0; i < n; i++) {
      if (i > 0) fprintf(port, " ");
      print_ptr_rec(port, p->buf[i], OUT);
    }

    fprintf(port, ")");
  } else if ((x & obj_mask) == string_tag) {
    if (state != IN) fprintf(port, "\"");

    string* p = (string*)(x-string_tag);
    unsigned long n = p->length >> fx_shift;
    unsigned long i;
    for (i = 0; i < n; i++) {
      int c = p->buf[i];
      if      (c == '"' ) fprintf(port, "\\\"");
      else if (c == '\\') fprintf(port, "\\\\");
      else                fputc(c, port);
    }

    if (state != IN) fprintf(port, "\"");
  } else if ((x & obj_mask) == symbol_tag) {
    print_ptr_rec(port, (x - symbol_tag) | string_tag, IN);
  } else if ((x & obj_mask) == closure_tag) {
    fprintf(port, "#<procedure>");
  } else {
    fprintf(port, "#<unknown 0x%08lx>", x);
  }
}

ptr ik_log(ptr msg) {
  fprintf(stderr, "log: ");
  print_ptr_rec(stderr, msg, IN);
  fprintf(stderr, "\n");
  return 0;
}

void ik_error(ptr x) {
  fprintf(stderr, "Exception");
  if ((x & obj_mask) == pair_tag) {
    ptr caller = ((cell*)(x-pair_tag))->car;
    ptr msg = ((cell*)(x-pair_tag))->cdr;
    if (caller != bool_f) {
      fprintf(stderr, " in ");
      print_ptr_rec(stderr, caller, OUT);
    }
    fprintf(stderr, ": ");
    print_ptr_rec(stderr, msg, IN);
  }
  fprintf(stderr, "\n");
  exit(0);
}

static void print_ptr(ptr x) {
  print_ptr_rec(stdout, x, OUT);
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
