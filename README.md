# An incremental scheme compiler [![Build Status](https://travis-ci.org/jaseemabid/inc.svg?branch=master)](https://travis-ci.org/jaseemabid/inc)

A tiny scheme to x86 asm compiler developed incrementally as described in the
paper [An Incremental Approach to Compiler Construction][paper] by Abdulaziz
Ghuloum.

## Getting started

    $ cargo build
    $ cargo test
    $ echo "(let ((a 1) (b 2)) (+ a b))" | cargo run -q

The generated assembly is usually easy to read

    $ echo "(let ((a 1) (b 2)) (+ a b))" | cargo run -q -- -S

## Usage

```txt
$ inc -h

Usage: ./inc [options]

Options:
    -o FILE             Output file name
    -S                  Print generated asm
    -p                  Print parse tree
    -P                  Dump raw parse tree
    -h, --help          print this help menu
```

## Docs

Inc is reasonably well documented and is preferably read with Cargo docs.

    $ cargo doc --document-private-items --open

- ℹ x86 module documentation contains links to a few good x86 tutorials.
- ℹ [How to C in 2016](https://matt.sh/howto-c) is a pretty good C refresher.
- ℹ [The Rust Programming language][book] book is a goods place to start
  learning rust. This project also uses a lot of iterators, so [Effectively
  Using Iterators In Rust][iter] might be useful as well

---

This project started in [Chez] but, I [ported it to Racket][rkt] and then again
to [rust]. The old project still lives at [rkt](./rkt).

[Chez]:  https://www.scheme.com
[paper]: docs/paper.pdf?raw=true
[rkt]:   https://github.com/jaseemabid/inc/commit/a8ab1e6c7506023e59ddcf11cfabe53fbaa5c00a
[rust]:  https://github.com/jaseemabid/inc/commit/cc333332a5f20dc9de168954808d363621bd0c97
[iter]:  https://hermanradtke.com/2015/06/22/effectively-using-iterators-in-rust.html
[book]:  https://doc.rust-lang.org/book/#the-rust-programming-language
