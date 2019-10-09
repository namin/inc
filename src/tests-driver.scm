;; Test runner for the incremental compiler
(define-syntax add-tests-with-string-output-noboot
  (syntax-rules (=>)
    [(_ test-name [expr => output-string]            ...)
     (set! all-tests
           (cons
            '(test-name [expr string  output-string noboot] ...)
            all-tests))]))

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr => output-string]            ...)
     (set! all-tests
           (cons
            '(test-name [expr string  output-string] ...)
            all-tests))]))

(define enable-boot-tests #f)

(define emit-program (lambda (expr) 0)) ;; re-implemented by compiler.scm

(define emit-library (lambda () #f)) ;; re-implemented by compiler.scm

(define (build)
  (unless (zero? (system "make stst --quiet"))
    (error 'make "Could not build target.")))

(define (execute fn)
  (unless (zero? (system (string-append "./stst >" fn)))
    (error 'make "Produced program exited abnormally.")))

(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format "Not an output port ~s." p)))
     p)))

(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))

(define runtime-file
  (make-parameter
   "startup.c"
   (lambda (fname)
     (unless (string? fname)
       (error 'runtime-file (format "Not a string ~s." fname)))
     fname)))

(define lib-file
  (make-parameter
   "lib.s"
   (lambda (fname)
     (unless (string? fname)
       (error 'lib-file (format "Not a string ~s." fname)))
     fname)))

(define (compile-lib)
  (let ([p (open-output-file (lib-file) 'replace)])
    (parameterize ([compile-port p])
      (emit-library))
    (close-output-port p)))

(define (get-string . args)
  (let ((fn (if (null? args) "stst.out" (car args))))
    (let ((port (open-input-file fn)))
      (let f ((r '()))
        (let ([c (read-char port)])
          (cond
            [(eof-object? c)
             (close-input-port port)
             (apply string (reverse r))]
            [else (f (cons c r))]))))))

;; Just like emit-program, but send generated asm to outfile
(define (compile-program expr)
  (let ([p (open-output-file "stst.s" 'replace)])
    (parameterize ([compile-port p])
      (emit-program expr))
    (close-output-port p)))

;; Compile and run a single expression, great for interactive devel
(define (run expr . args)
  (let ((fn (if (null? args) "stst.out" (car args))))
    (compile-program expr)
    (build)
    (execute fn)
    (get-string fn)))

(define (boot-run expr)
  (let ([p (open-output-file "stst.scm" 'replace)])
      (write expr p)
      (close-output-port p))
  (system "./boot <stst.scm >stst.out")
  (let ((r (get-string)))
    (substring r 0 (- (string-length r) 3))))

(define all-tests '())

(define (test-with-string-output test-id expr expected-output noboot)
  (unless (string=? expected-output (run expr))
    (error 'test (format "Output mismatch for test ~s, expected ~s, got ~s."
                         test-id expected-output (get-string))))
  (when enable-boot-tests
    (unless noboot
      (let ((r (boot-run expr)))
        (unless (string=? expected-output r)
          (error 'test (format "boot: Output mismatch for test ~s, expected ~s, got ~s."
                               test-id expected-output r))))))
  )

(define (test-one test-id test)
  (let ([expr (car test)]
        [type (cadr test)]
        [out  (caddr test)]
        [noboot (if (null? (cdddr test)) #f (eq? 'noboot (cadddr test)))])
    (printf "Test ~s:~s ..." test-id expr)
    (flush-output-port)
    (case type
     [(string) (test-with-string-output test-id expr out noboot)]
     [else (error 'test (format "Invalid test type ~s." type))])
    (printf " Ok.\n")))

(define (test-all)
  (let f ([i 0] [ls (reverse all-tests)])
    (if (null? ls)
        (printf "Passed all ~s tests.\n" i)
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
