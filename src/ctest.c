#include "startup.h"

static int global_foo = 1;

char inc(char c) {
  return c+1;
}

static ptr global_fun = 1;

char do_call(char c, char (*fun)(char)) {
  return fun(c);
}

char fancy(char c) {
  return do_call(c, &inc);
}
ptr scheme_entry(context* ctxt, char* stack_base, memory* mem) {
  char* heap = mem->heap_next;
  if (global_fun == 0) {
    global_fun = ((ptr) &inc) | closure_tag;
  }
  stack_base[-1] = 110 + global_foo;
  heap[0] = 'a';
  heap[1] = fancy(heap[0]);
  heap[2] = global_fun;
  ptr p = 0 | (heap[0] << char_shift) | char_tag;
  return p;
}
