# An incremental scheme compiler

<!-- [![Build Status](https://travis-ci.org/jaseemabid/inc.svg?branch=master)](https://travis-ci.org/jaseemabid/inc) -->
<!-- [![Docker Repository on Quay](https://quay.io/repository/jaseemabid/inc/status "Docker Repository on Quay")](https://quay.io/repository/jaseemabid/inc) -->

A tiny scheme to x86 asm compiler developed incrementally as described in the
paper [An Incremental Approach to Compiler Construction][paper] by Abdulaziz
Ghuloum.

## Getting started

    $ cargo build
    $ cargo test
    $ echo "(let ((a 1) (b 2)) (+ a b)) | cargo run -q

## Docs

Inc is reasonably well documented and is preferably read with Cargo docs.

    $ cargo doc --document-private-items --open

## ~

This project started in [Chez Scheme] but, I [ported it to Racket][rkt] and then
again to [rust]. The old project still lives at [rkt](./rkt).

[Chez Scheme]:  https://www.scheme.com
[paper]:        docs/paper.pdf?raw=true
[rkt]:          https://github.com/jaseemabid/inc/commit/a8ab1e6c7506023e59ddcf11cfabe53fbaa5c00a
[rust]:         https://github.com/jaseemabid/inc/commit/cc333332a5f20dc9de168954808d363621bd0c97
