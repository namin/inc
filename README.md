# An incremental scheme compiler

<!-- [![Build Status](https://travis-ci.org/jaseemabid/inc.svg?branch=master)](https://travis-ci.org/jaseemabid/inc) -->
<!-- [![Docker Repository on Quay](https://quay.io/repository/jaseemabid/inc/status "Docker Repository on Quay")](https://quay.io/repository/jaseemabid/inc) -->

A tiny scheme to x86 asm compiler developed incrementally as described in the
paper [An Incremental Approach to Compiler Construction][paper] by Abdulaziz
Ghuloum.

This project started in [Chez Scheme] but, I [ported it to Racket][rkt] and then
again to [rust]. The old project still lives at [rkt](./rkt).

## Getting started

Cargo docs can open up the documentation in your browser. The top level explains
how to navigate the code.

    $ cargo doc --document-private-items  --open

## Required reading

- [Writing 64 Bit Assembly on Mac OS X][1]

## Testing

    $ cargo test

Inc takes testing **very** seriously, primarily because it is very painful to
test a compiler manually and the untyped nature of scheme makes it very easy to
mess things up.

[1]:            https://www.idryman.org/blog/2014/12/02/writing-64-bit-assembly-on-mac-os-x/
[Chez Scheme]:  https://www.scheme.com
[paper]:        docs/paper.pdf?raw=true
[rkt]:          https://github.com/jaseemabid/inc/commit/a8ab1e6c7506023e59ddcf11cfabe53fbaa5c00a
[rust]:         https://github.com/jaseemabid/inc/commit/cc333332a5f20dc9de168954808d363621bd0c97
