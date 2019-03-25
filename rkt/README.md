# An incremental scheme compiler in Racket

A tiny scheme to x86 asm compiler developed incrementally as described in the
paper [An Incremental Approach to Compiler Construction][1] by Abdulaziz
Ghuloum.

:warning: This port is now abandoned, development continues on the Rust
implementation.

NOTE: Mac users should stick with Docker for now.

    $ make test

The Dockerfile should make it easier to get started.

    $ docker build . -t inc
    $ docker run -it inc
    /inc $ make test

## Testing

Inc takes testing **very** seriously, primarily because it is very painful to
test a compiler manually and the untyped nature of scheme makes it very easy to
mess things up.

As of now, the compiler including the runtime is just about 785 lines of code,
but there are over 400 tests for it in 630 lines of code. See
[src/tests-driver.scm](src/tests-driver.scm) for docs on how the tests are
defined and run.

[1]: https://github.com/namin/inc/blob/master/docs/paper.pdf?raw=true
