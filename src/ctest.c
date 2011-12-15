#include "startup.h"

ptr scheme_entry(char* stack_base){
  *(stack_base-8) = 7;
  return *(stack_base-8) << 2;
}
