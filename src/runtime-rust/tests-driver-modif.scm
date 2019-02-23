(define (build)
  (unless (zero? (system "cd runtime-rust; cargo build --target i686-unknown-linux-gnu --quiet; cd .."))
    (error 'make "Could not build target.")))

(define (execute)
  (unless (zero? (system "./runtime-rust/target/i686-unknown-linux-gnu/debug/runtime-rust"))
    (error 'make "Produced program exited abnormally.")))
