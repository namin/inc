# Generate the testable executable inc
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

src/inc: src/inc.s src/runtime.c
	cd src && \
	gcc	-m64 \
		-g3 -ggdb3 \
		-fomit-frame-pointer \
		-fno-asynchronous-unwind-tables \
		-O0 runtime.c inc.s \
		-o inc

.PHONY: inc
inc: src/inc

.PHONY: test
test:
	cd src && echo '(test-all)' | scheme compiler.scm --quiet

.PHONY: clean
clean:
	cd src && rm -f inc.s inc inc.out
