stst: stst.s startup.c
	gcc -m64 -g3 -ggdb3 -fno-omit-frame-pointer -O0 startup.c stst.s -o stst

.PHONY: clean
clean:
	rm -f stst.s stst stst.out
