# An incremental scheme compiler

A tiny scheme to x86 asm compiler developed incrementally as described in the
paper [An Incremental Approach to Compiler Construction][1] by Abdulaziz
Ghuloum.

The code is tested with Chez Scheme 9.4.

    > (load  "compiler.asm")
    > (test-all)


[1]: https://github.com/namin/inc/blob/master/docs/paper.pdf?raw=true