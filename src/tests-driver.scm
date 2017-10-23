;; Test runner for the incremental compiler

(define (emit-library) #f) ;; (optionally) implemented by compiler.scm

(define (build)
  (unless (zero? (system "make stst --quiet"))
    (error 'make "Could not build target")))

(define (execute)
  (unless (zero? (system "./stst > stst.out"))
    (error 'make "Produced program exited abnormally")))

(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format "not an output port ~s" p)))
     p)))

(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))

(define runtime-file
  (make-parameter
   "startup.c"
   (lambda (fname)
     (unless (string? fname)
       (error 'runtime-file (format "not a string ~s" fname)))
     fname)))

(define lib-file
  (make-parameter
   "lib.s"
   (lambda (fname)
     (unless (string? fname)
       (error 'lib-file (format "not a string ~s" fname)))
     fname)))

(define (compile-lib)
  (let ([p (open-output-file (lib-file) 'replace)])
    (parameterize ([compile-port p])
      (emit-library))
    (close-output-port p)))

(define (get-string)
  (with-output-to-string
    (lambda ()
      (with-input-from-file "stst.out"
        (lambda ()
          (let f ()
            (let ([c (read-char)])
              (cond
               [(eof-object? c) (void)]
               [else (display c) (f)]))))))))

;; Just like emit-program, but send generated asm to outfile
(define (compile-program expr)
  (let ([p (open-output-file "stst.s" 'replace)])
    (parameterize ([compile-port p])
      (emit-program expr))
    (close-output-port p)))

;; Compile and run a single expression, great for interactive devel
(define (run expr)
  (compile-program expr)
  (build)
  (execute)
  (get-string))

(define all-tests '())

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr => output-string]            ...)
     (set! all-tests
           (cons
            '(test-name [expr string  output-string] ...)
            all-tests))]))

(define (test-with-string-output test-id expr expected-output)
  (unless (string=? expected-output (run expr))
    (error 'test (format "Output mismatch for test ~s, expected ~s, got ~s"
                         test-id expected-output (get-string)))))

(define (test-one test-id test)
  (let ([expr (car test)]
        [type (cadr test)]
        [out  (caddr test)])
    (printf "Test ~s:~s ..." test-id expr)
    (flush-output-port)
    (case type
     [(string) (test-with-string-output test-id expr out)]
     [else (error 'test (format "invalid test type ~s" type))])
    (printf " ok\n")))

(define (test-all)
  (let f ([i 0] [ls (reverse all-tests)])
    (if (null? ls)
        (printf "Passed all ~s tests\n" i)
        (let ([x (car ls)] [ls (cdr ls)])
          (let* ([test-name (car x)]
                 [tests (cdr x)]
                 [n (length tests)])
            (printf "Performing ~a tests ...\n" test-name)
            (let g ([i i] [tests tests])
              (cond
                [(null? tests) (f i ls)]
                [else
                 (test-one i (car tests))
                 (g (add1 i) (cdr tests))])))))))
