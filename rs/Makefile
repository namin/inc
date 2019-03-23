.PHONY:run
run:
	@./a.out

.PHONY: inc.s
inc.s:
	@cargo run -q inc > inc.s

a.out: inc.s runtime.c
	@clang -m64 \
		  -g3 -ggdb3 \
		  -fomit-frame-pointer \
		  -fno-asynchronous-unwind-tables \
		  -O0 runtime.c inc.s
