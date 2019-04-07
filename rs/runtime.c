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

#define numtag   0
#define booltag  1
#define chartag  2
#define pairtag  3
#define niltag   4
#define strtag   5
#define heaptag  7

#define shift    3
#define mask     0b00000111

int64_t bool_f = (0 << shift) | booltag;
int64_t bool_t = (1 << shift) | booltag;

void print(int64_t val) {

    if ((val & mask) == numtag) {
        printf("%" PRId64, val >> shift);

    } else if ((val & mask) == chartag) {
        char c = val >> shift;

        if      (c == '\t') printf("#\\tab");
        else if (c == '\n') printf("#\\newline");
        else if (c == '\r') printf("#\\return");
        else if (c == ' ')  printf("#\\space");
        else                printf("#\\%c", c);

    } else if (val == bool_t) {
        printf("#t");

    } else if (val == bool_f) {
        printf("#f");

    } else if (val == niltag) {
        printf("()");

    } else {
        printf("ERROR %" PRId64, val);
    }
}

int main() {

    FILE *debug = getenv("DEBUG") ? stderr : fopen("/dev/null", "w");
    fprintf(debug, "%s\n\n", "The glorious incremental compiler");

    int64_t val = init();

    int64_t rsi;

    // Copy the value of RSI into a local variable. The nop instruction makes it
    // easier to spot this in the generated asm
    asm ("nop; movq %%rsi, %0" : "=r" (rsi));

    print(val);
    printf("\n");
}
