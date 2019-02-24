# runtime-rust

This is an (abandoned?) attempt to rewrite `startup.c` in Rust.

All the tests below `4.*` pass, but there is more work to reach
parity, including implementing the garbage collector.

In Scheme, from `..`:
```
(load "compiler.scm")
(load "runtime-rust/tests-driver-modif.scm")`
(test-al)```

See `runtime-rust/tests-driver-modif.scm` for how to build and run.
