#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// The default calling convention used by GCC on x86-64 seems to be System V
// AMD64 ABI, in which arguments are passed in the registers RDI, RSI, RDX, RCX,
// R8, R9 and the return value is passed back in RAX.
extern int64_t init() __attribute__((noinline));

int main() {
    int64_t val = init();
    printf("%" PRId64 "\n", val);
}
