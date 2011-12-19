#include "startup.h"

ptr scheme_entry(context* ctxt, char* stack_base, char* heap){
  heap[0] = 'a';
  heap[1] = heap[0];
  ptr p = 0 | (heap[0] << char_shift) | char_tag;
  return p;
}
