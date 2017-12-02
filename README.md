# An incremental scheme compiler [![Build Status](https://travis-ci.org/jaseemabid/inc.svg?branch=master)](https://travis-ci.org/jaseemabid/inc)

A tiny scheme to x86 asm compiler developed incrementally as described in the
paper [An Incremental Approach to Compiler Construction][1] by Abdulaziz
Ghuloum.

The code is tested with Chez Scheme 9.5. You can install chez on mac with `brew
install chezscheme`


    $ make test

The Dockerfile should make it easier to get started.

    $ docker build . -t inc
    $ docker run -it inc
    /inc $ make test

[1]: https://github.com/namin/inc/blob/master/docs/paper.pdf?raw=true
