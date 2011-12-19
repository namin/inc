#include "startup.h"

char inc(char c) {
  return c+1;
}

char do_call(char c, char (*fun)(char)) {
  return fun(c);
}

char fancy(char c) {
  return do_call(c, &inc);
}
ptr scheme_entry(context* ctxt, char* stack_base, char* heap){
  stack_base[-1] = 111;
  heap[0] = 'a';
  heap[1] = fancy(heap[0]);
  ptr p = 0 | (heap[0] << char_shift) | char_tag;
  return p;
}
