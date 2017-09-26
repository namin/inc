#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

// The default calling convention used by GCC on x86-64 seems to be System V
// AMD64 ABI, in which arguments are passed in the registers RDI, RSI, RDX, RCX,
// R8, R9 and the return value is passed back in RAX.
extern int init(int64_t*) __attribute__((noinline));

#define bool_f     0b00101111
#define bool_t     0b01101111
#define boolshift           7
#define charmask   0b00111111
#define fxmask     0b00000011
#define charshift           8
#define chartag    0b00001111
#define fxshift             2
#define fxtag               0
#define list_nil   0b00111111

void print(int64_t val) {

    if ((val & fxmask) == fxtag) {
        printf("%" PRId64 "\n", val >> fxshift);

    } else if ((val & charmask) == chartag) {
        char c = val >> charshift;

        if      (c == '\t') printf("#\\tab\n");
        else if (c == '\n') printf("#\\newline\n");
        else if (c == '\r') printf("#\\return\n");
        else if (c == ' ')  printf("#\\space\n");
        else                printf("#\\%c\n", c);

    } else if (val == bool_t) {
        printf("#t\n");

    } else if (val == bool_f) {
        printf("#f\n");

    } else if (val == list_nil) {
        printf("()\n");

    } else {
        printf("ERROR\n");
    }
}

int main() {

    int64_t *heap = calloc(1024, 8);
    int64_t val = init(heap);

    print(val);
    free(heap);
}
