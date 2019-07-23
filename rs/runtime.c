#include <inttypes.h>
#include <signal.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

// The default calling convention used by GCC on x86-64 seems to be System V
// AMD64 ABI, in which arguments are passed in the registers RDI, RSI, RDX, RCX,
// R8, R9 and the return value is passed back in RAX.
extern int64_t init(int64_t*) __attribute__((noinline));

#define numtag   0
#define booltag  1
#define chartag  2
#define pairtag  3
#define niltag   4
#define strtag   5

#define shift    3
#define mask     0b00000111

int64_t bool_f = (0 << shift) | booltag;
int64_t bool_t = (1 << shift) | booltag;

void print(int64_t val, bool nested) {

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

    } else if ((val & mask) == pairtag) {
        int64_t *p = (int64_t *)(val - pairtag);
        int64_t car = *p;
        int64_t cdr = *(p + 1);

        if (!nested) printf("(");

        print(car, false);

        if (cdr != niltag) {
            if ((cdr & mask) != pairtag) {
                printf(" . ");
                print(cdr, false);
            } else {
                printf(" ");
                print(cdr, true);
            }
        }
        if (!nested) printf(")");
    } else if ((val & mask) == strtag) {
        // This is why C is unsafe, but that is exactly what is letting me do
        // this sort of custom memory management.
        //
        // A string in memory is a pair of length and a pointer to a blob of
        // bytes - ideally guaranteed by the compiler to be valid UTF-8. See
        // compiler module for documentation on the layout.
        int64_t *p = (int64_t *)(val - strtag);
        int64_t len = *(p + 0);
        int64_t *str = p + 1;

        printf("\"");
        fwrite((void *)str, 1, len, stdout);
        printf("\"");

    } else {
        printf("ERROR; unknown value at reference %p \n", (uintptr_t *)val);
    }
}

void set_handler(void (*handler)(int,siginfo_t *,void *)) {
    struct sigaction action;
    action.sa_flags = SA_SIGINFO;
    action.sa_sigaction = handler;

    if (sigaction(SIGSEGV, &action, NULL) == -1) {
        perror("sigsegv: sigaction");
        _exit(1);
    }
}

void handler(int signo, siginfo_t *info,   __attribute__((unused)) void *extra) {
    printf("Program crashed due to unexpected error \n");
    printf("Signal number        : %d \n", signo);
    printf("SIGSEGV at address   : 0x%lx \n",(long) info->si_addr);
    abort();
}

int main() {

    FILE *debug = getenv("DEBUG") ? stderr : fopen("/dev/null", "w");
    fprintf(debug, "%s\n\n", "The glorious incremental compiler");

    set_handler(handler);

    int64_t rsi, rsp;
    int64_t *heap = calloc(1024, 8);

    // Read current stack pointer into local variable for diagnostics
    asm ("nop; movq %%rsp, %0" : "=r" (rsp));

    fprintf(debug, "Heap initialized at : %p  \n", heap);
    fprintf(debug, "Stack begins at     : %p  \n", (uintptr_t *)rsp );

    // Execute all of the generated ASM; this could return a value or segfault
    int64_t val = init(heap);

    // Copy the value of RSI into a local variable. The nop instruction makes it
    // easier to spot this in the generated asm
    asm ("nop; movq %%rsi, %0" : "=r" (rsi));

    ptrdiff_t size = (uintptr_t)rsi - (uintptr_t)heap;

    fprintf(debug, "HEAP Segment        : %p -> %p \n" , heap, (int64_t *)rsi);
    fprintf(debug, "HEAP size           : %td bytes\n", size);
    fprintf(debug, "Result              : ");
    fflush(stdout);

    print(val, false);
    printf("\n");

    free(heap);
}
