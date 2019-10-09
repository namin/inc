(load "compiler.scm")

(compile-program
 '(let ()
    (load "self.scm")
    (load "reader.scm")
    (load "compiler.scm")
    (let loop ((expr (read (current-input-port))))
      (if (eof-object? expr)
          'Ok
          (begin
            (display (run expr "boot.out"))
            (loop (read (current-input-port))))))))
