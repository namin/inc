# runtime-rust

This is an attempt to rewrite `startup.c` in Rust, as an exercise in learning Rust.
Pointless?

All the tests below `5.*` pass.
The remaining TODO to reach parity is to implement the garbage collector.

In Scheme, from `..`:
```
(load "compiler.scm")
(load "runtime-rust/tests-driver-modif.scm")`
(test-al)```

See `runtime-rust/tests-driver-modif.scm` for how to build and run.
