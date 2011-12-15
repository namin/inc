#include "startup.h"

ptr scheme_entry(){
  ptr x = 7 << 2; /* represents fixnum 7 */
  return ((((x & fx_mask) == fx_tag) << bool_bit) | bool_f); /* should print #t */
}
