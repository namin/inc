#include "startup.h"

ptr scheme_entry(context* ctxt, char* stack_base, char* heap){
  ctxt->ecx = (void*) 2;
  *(stack_base-8) = 7;
  return *(stack_base-8) << (long) ctxt->ecx;
}
