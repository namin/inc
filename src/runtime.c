#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// The default calling convention used by GCC on x86-64 seems to be System V
// AMD64 ABI, in which arguments are passed in the registers RDI, RSI, RDX, RCX,
// R8, R9 and the return value is passed back in RAX.
extern int64_t init(int64_t*) __attribute__((noinline));

#define bool_f     0b00101111
#define bool_t     0b01101111
#define boolshift           7
#define charmask   0b00111111
#define charshift           8
#define chartag    0b00001111
#define fxmask     0b00000011
#define fxshift             2
#define fxtag               0
#define list_nil   0b00111111
#define pairtag    0b00000001
#define heapmask   0b00000111


void print(int64_t val, bool nested) {

    if ((val & fxmask) == fxtag) {
        printf("%" PRId64, val >> fxshift);

    } else if ((val & charmask) == chartag) {
        char c = val >> charshift;

        if      (c == '\t') printf("#\\tab");
        else if (c == '\n') printf("#\\newline");
        else if (c == '\r') printf("#\\return");
        else if (c == ' ')  printf("#\\space");
        else                printf("#\\%c", c);

    } else if (val == bool_t) {
        printf("#t");

    } else if (val == bool_f) {
        printf("#f");

    } else if (val == list_nil) {
        printf("()");

    } else if ((val & heapmask) == pairtag) {
        int64_t *p = (int64_t *)(val - 1);
        int64_t car = *p;
        int64_t cdr = *(p + 1);

        if (!nested) printf("(");

        print(car, false);

        if (cdr != list_nil) {
            if ((cdr & heapmask) != pairtag) {
                printf(" . ");
                print(cdr, false);
            } else {
                printf(" ");
                print(cdr, true);
            }
        }
        if (!nested) printf(")");
    } else {
        printf("ERROR");
    }
}

int main() {

    int64_t *heap = calloc(1024, 8);
    int64_t val = init(heap);

    // printf("HEAP %p  \n", heap);

    print(val, false);
    printf("\n");

    free(heap);
}
