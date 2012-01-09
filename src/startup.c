#include <assert.h>
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

ptr s_fflush(ptr fd) {
  fflush(fdopen(unshift(fd), "w"));
  return 0;
}

ptr scheme_write(ptr fd, ptr x, ptr opt) {
  FILE* port = fdopen(unshift(fd), "w");
  print_ptr_rec(port, x, unshift(opt));
  fflush(port);
  return 0;
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

static char* gc_next;
static ptr* gc_queue;
static char* gc_new_heap_base;
static char* gc_new_heap_top;

static char* gc_get_forward_pointer(char* p) {
  ptr x = (ptr)*p;
  if (x != gc_forward_mark)
    return NULL;

  char* q = (char*) *(((ptr*)p)+1);
  assert(gc_new_heap_base <= q && q < gc_new_heap_top);
  return q;
}

static void gc_set_forward_pointer(char* p, char* q) {
  assert(gc_new_heap_base <= q && q < gc_new_heap_top);
  *p = gc_forward_mark;
  *(((ptr*)p)+1) = (ptr)q;
}

static unsigned int gc_align(unsigned int n) {
  unsigned int cell = 1 << obj_shift;
  return ((n + cell - 1)/cell) * cell;
}

static int gc_ptr_object(ptr x) {
  ptr tag = x & obj_mask;
  switch (tag) {
  case pair_tag:
  case vector_tag:
  case symbol_tag:
  case string_tag:
  case closure_tag:
    return 1;
  default:
    return 0;
  }
}

static unsigned int gc_size(ptr x) {
  unsigned int n = 0;
  ptr tag = x & obj_mask;
  char* p = (char*)(x-tag);
  switch (tag) {
  case pair_tag:
    n = 2 << word_shift;
    break;
  case vector_tag:
    n = (((vector*)p)->length >> fx_shift) + 1;
    n = n << word_shift;
    break;
  case symbol_tag:
  case string_tag:
    n = (((string*)p)->length >> fx_shift) + word_size;
    break;
  case closure_tag:
    n = 0;
    while (((closure*)p)->fvs[n++] != closure_end);
    n++;
    n = n << word_shift;
    break;
  }
  return n;
}

static ptr gc_forward(ptr x) {
  if (!gc_ptr_object(x))
    return x;

  ptr tag = x & obj_mask;
  char* p = (char*)(x-tag);
  char* q = gc_get_forward_pointer(p);
  if (q != NULL) {
    assert((((ptr)q) & obj_mask) == 0);
    return ((ptr)q) | tag;
  }

  q = gc_next;
  unsigned int n = gc_size(x);
  unsigned int i = 0;
  for (i=0; i<n; i++)
    gc_next[i] = p[i];
  gc_next = gc_next + gc_align(n);
  gc_set_forward_pointer(p, q);
  assert((((ptr)q) & obj_mask) == 0);
  ptr f = ((ptr)q) | tag;
  *(gc_queue++) = f;
  return f;
}

static void gc_clean_new() {
  char* p = gc_new_heap_base;
  while (p < gc_new_heap_top) {
    *p = 0;
    p++;
  }
}

static void gc(memory* mem, char* stack) {
  gc_new_heap_base = mem->heap_base_alt;
  gc_new_heap_top = mem->heap_top_alt;
  gc_clean_new();

  gc_next = mem->heap_base_alt;
  gc_queue = (ptr*)(mem->scratch_base);

  ptr* scan = gc_queue;
  ptr* root = (ptr*)mem->global_base;
  while (root < (ptr*)mem->global_next) {
    *root = gc_forward(*root);
    root++;
  }

  root = (ptr*)mem->stack_base;
  root--; // skip top-level return addresses
  root--;
  while (root >= (ptr*) stack) {
    if (*root == return_addr) root--; // skip return address
    else *root = gc_forward(*root);
    root--;
  }

  mem->edi = gc_forward(mem->edi);

  while (scan < gc_queue) {
    ptr x = *(scan++);

    unsigned int n = gc_size(x);
    assert(n != 0);

    ptr tag = x & obj_mask;
    char* q = (char*)(x-tag);
    assert(gc_new_heap_base <= q && q < gc_new_heap_top);

    if (tag == pair_tag) {
      cell* p = (cell*)(x-tag);
      p->car = gc_forward(p->car);
      p->cdr = gc_forward(p->cdr);
    } else if (tag == vector_tag) {
      vector* p = (vector*)(x-tag);
      unsigned int len = p->length >> fx_shift;
      unsigned int i;
      for (i=0; i<len; i++)
	p->buf[i] = gc_forward(p->buf[i]);
    } else if (tag == closure_tag) {
      closure* p = (closure*)(x-tag);
      unsigned int i = 0;
      while (p->fvs[i] != closure_end) {
	p->fvs[i] = gc_forward(p->fvs[i]);
	i++;
      }
    }
  }

  mem->heap_next = gc_next;
  mem->heap_base_alt = mem->heap_base;
  mem->heap_top_alt = mem->heap_top;
  mem->heap_base = gc_new_heap_base;
  mem->heap_top = gc_new_heap_top;
}

char* heap_alloc(memory* mem, char* stack, int size) {
  char* heap_next = mem->heap_next;
  char* heap_new = heap_next + size;
  if (heap_new >= mem->heap_top) {
    gc(mem, stack);
    heap_next = mem->heap_next;
    heap_new = heap_next + size;
    if (heap_new >= mem->heap_top) {
      fprintf(stderr, "Exception: overflow\n");
      exit(0);
    }
  }
  mem->heap_next = heap_new;
  return heap_next;
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
  int heap_size = (4 * 16 * 4096);
  int global_size = (16 * 4096);
  int scratch_size = (16 * 4096);

  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;

  char* heap = allocate_protected_space(heap_size);
  char* global = allocate_protected_space(global_size);
  char* scratch = allocate_protected_space(scratch_size);

  context ctxt;

  memory mem;
  mem.heap_next = heap;
  mem.global_next = global;
  mem.heap_base = heap;
  mem.heap_top = heap + heap_size/2;
  mem.heap_base_alt = mem.heap_top;
  mem.heap_top_alt = heap + heap_size;
  mem.global_base = global;
  mem.stack_base = stack_base;
  mem.scratch_base = scratch;

  print_ptr(scheme_entry(&ctxt, stack_base, &mem));

  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap, stack_size);
  return 0;
}
