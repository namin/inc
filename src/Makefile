# Generate the testable executable inc
#
# -m32 forces to compile for 32bit target, this prevents accidental surprises.
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

lib.s: tests-driver.scm compiler.scm lib.scm
	echo '(compile-lib)' | scheme compiler.scm --quiet

bootlib.s: bootlib.scm tests-driver.scm compiler.scm self.scm reader.scm startup.c lib.s
	echo "" | scheme bootlib.scm --quiet
	make stst
	./stst

boot: repl.scm tests-driver.scm compiler.scm self.scm reader.scm startup.c lib.s
	echo "" | scheme repl.scm --quiet
	make stst
	mv stst boot

.PHONY: stst
stst: startup.c lib.s stst.s
	gcc	-m32 \
		-Wall \
		-g3 -ggdb3 \
		-fomit-frame-pointer \
		-fno-asynchronous-unwind-tables \
		-O0 startup.c lib.s stst.s \
		-o stst

.PHONY: test
test: lib.s
	echo '(test-all)' | scheme compiler-tests.scm --quiet

.PHONY: clean
clean:
	rm -f boot stst.s lib.s stst stst.out
