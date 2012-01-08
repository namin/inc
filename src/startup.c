#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

#include "startup.h"
#include "scheme_entry.h"

#define DISPLAY 2
#define IN 1
#define OUT 0

#define FILENAME_MAX_LENGTH 100

static void print_ptr_rec(FILE* port, ptr x, int state) {
  if ((x & fx_mask) == fx_tag) {
    fprintf(port, "%d", ((int) x) >> fx_shift);
  } else if (x == bool_f) {
    fprintf(port, "#f");
  } else if (x == bool_t) {
    fprintf(port, "#t");
  } else if (x == list_nil) {
    fprintf(port, "()");
  } else if (x == eof_obj) {
    fprintf(port, "#!eof");
  } else if ((x & char_mask) == char_tag) {
    char c = (char) (x >> char_shift);
    if (state == DISPLAY) {
      fputc(c, port);
    } else {
      if      (c == '\t') fprintf(port, "#\\tab");
      else if (c == '\n') fprintf(port, "#\\newline");
      else if (c == '\r') fprintf(port, "#\\return");
      else if (c == ' ')  fprintf(port, "#\\space");
      else                fprintf(port, "#\\%c", c);
    }
  } else if ((x & obj_mask) == pair_tag) {
    if (state == OUT) fprintf(port, "(");
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
    if (state == OUT) fprintf(port, ")");
  } else if ((x & obj_mask) == vector_tag) {
    fprintf(port, "#(");

    vector* p = (vector*)(x-vector_tag);
    unsigned int n = p->length >> fx_shift;
    unsigned int i;
    for (i = 0; i < n; i++) {
      if (i > 0) fprintf(port, " ");
      print_ptr_rec(port, p->buf[i], OUT);
    }

    fprintf(port, ")");
  } else if ((x & obj_mask) == string_tag) {
    if (state == OUT) fprintf(port, "\"");

    string* p = (string*)(x-string_tag);
    unsigned int n = p->length >> fx_shift;
    unsigned int i;
    for (i = 0; i < n; i++) {
      int c = p->buf[i];
      if      (c == '"' ) fprintf(port, "\\\"");
      else if (c == '\\') fprintf(port, "\\\\");
      else                fputc(c, port);
    }

    if (state == OUT) fprintf(port, "\"");
  } else if ((x & obj_mask) == symbol_tag) {
    print_ptr_rec(port, (x - symbol_tag) | string_tag, IN);
  } else if ((x & obj_mask) == closure_tag) {
    fprintf(port, "#<procedure>");
  } else {
    fprintf(port, "#<unknown 0x%08x>", x);
  }
}

static void print_ptr(ptr x) {
  print_ptr_rec(stdout, x, OUT);
  printf("\n");
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

static int unshift(ptr x) {
  return ((int) x) >> fx_shift;
}

static ptr shift(int x) {
  return x << fx_shift;
}

static char* string_data(ptr x) {
  string* p = (string*)(x-string_tag);
  return p->buf;  
}

static void cp_str_data(ptr x, char* buf, int buf_length) {
  string* p = (string*)(x-string_tag);
  unsigned int n = p->length >> fx_shift;
  unsigned int i;
  for (i = 0; i < n || i < buf_length-1; i++) {
    buf[i] = p->buf[i];
  }
  buf[i] = '\0';
}

ptr s_write(ptr fd, ptr str, ptr len) {
  int bytes = write(unshift(fd),
		    string_data(str),
		    unshift(len));
  return shift(bytes);
}

void s_fflush(ptr fd) {
  fflush(fdopen(unshift(fd), "w"));
}

void scheme_write(ptr fd, ptr x, ptr opt) {
  FILE* port = fdopen(unshift(fd), "w");
  print_ptr_rec(port, x, unshift(opt));
  fflush(port);
}

ptr s_open_write(ptr fname) {
  char c_fname[FILENAME_MAX_LENGTH];
  cp_str_data(fname, c_fname, FILENAME_MAX_LENGTH);
  int fd = open(c_fname, O_WRONLY | O_CREAT | O_TRUNC, 0640);
  return shift(fd);
}


ptr s_open_read(ptr fname) {
  char c_fname[FILENAME_MAX_LENGTH];
  cp_str_data(fname, c_fname, FILENAME_MAX_LENGTH);
  int fd = open(c_fname, O_RDONLY);
  return shift(fd);
}

ptr s_read_char(ptr fd) {
  char ca[1];
  if ((read(unshift(fd), ca, 1)) < 1)
    return eof_obj;
  return (ca[0] << char_shift) | char_tag;
}

ptr s_close(ptr fd) {
  return shift(close(unshift(fd)));
}

char* heap_alloc(memory* mem, char* stack, int size) {
  char* heap_cur = mem->heap_cur;
  char* heap_new = heap_cur + size;
  if (heap_new >= mem->heap_top) {
    fprintf(stderr, "Exception: overflow");
    exit(0);
  }
  mem->heap_cur = heap_new;
  return heap_cur;
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
  int stack_size = (16 * 4096);
  int heap_size = (128 * 16 * 4096);
  int global_size = (16 * 4096);

  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;

  char* heap = allocate_protected_space(heap_size);

  char* global = allocate_protected_space(global_size);

  context ctxt;

  memory mem;
  mem.heap_cur = heap;
  mem.global_cur = global;
  mem.heap_base = heap;
  mem.heap_top = heap + heap_size;
  mem.global_base = global;
  mem.stack_base = stack_base;

  print_ptr(scheme_entry(&ctxt, stack_base, &mem));

  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap, stack_size);
  return 0;
}
