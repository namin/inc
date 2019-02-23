(define (build)
  (unless (zero? (system "rm ./runtime-rust/target/i686-unknown-linux-gnu/debug/runtime-rust; cd runtime-rust; cargo build --target i686-unknown-linux-gnu --quiet; cd .."))
    (error 'make "Could not build target.")))

(define (execute)
  (unless (zero? (system "./runtime-rust/target/i686-unknown-linux-gnu/debug/runtime-rust >stst.out"))
    (error 'make "Produced program exited abnormally.")))
