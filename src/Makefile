# Generate the testable executable stst
#
# -m64 forces to compile for 64bit target, this prevents accidental surprises.
#
# `-g3 -ggdb3` generates as much debug symbols as possible, notably the latter
# allows the use of macros in gdb prompt. As of now, only GCC seems to support
# this option.
#
# Omitting the frame pointer with `-fomit-frame-pointer` removes the standard
# function preamble and post when not needed. This makes the assembly slightly
# easier to read and harder to debug.
#
# `-fno-asynchronous-unwind-tables` gets rid of all the '.cfi' directives from
# the generated asm.

stst: stst.s startup.c
	gcc	-m64 \
		-g3 -ggdb3 \
		-fomit-frame-pointer \
		-fno-asynchronous-unwind-tables \
		-O0 startup.c stst.s \
		-o stst

.PHONY: clean
clean:
	rm -f stst.s stst stst.out
