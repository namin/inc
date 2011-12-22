(define all-tests '())

(define (compile-program x)
  (emit-program x))        ;; implemented by compiler.scm
(define (emit-library) #f) ;; (optionally) implemented by compiler.scm

(define-syntax add-tests-with-string-output
  (syntax-rules (=>)
    [(_ test-name [expr => output-string] ...)
     (set! all-tests
        (cons 
           '(test-name [expr string  output-string] ...)
            all-tests))]))

(define (build)
  (unless (zero? (system (format "gcc -m32 -Wall -o stst ~a ~a stst.s"
                                 (runtime-file)
                                 (lib-file))))
    (error 'make "could not build target")))

(define (execute)
  (unless (zero? (system "./stst > stst.out"))
    (error 'make "produced program exited abnormally")))


(define (build-program expr)
   (run-compile expr)
   (build))

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

(define (test-with-string-output test-id expr expected-output)
   (run-compile expr)
   (build)
   (execute)
   (unless (string=? expected-output (get-string))
     (error 'test (format "output mismatch for test ~s, expected ~s, got ~s"
        test-id expected-output (get-string)))))

(define (test-one test-id test)
  (let ([expr (car test)]
        [type (cadr test)]
        [out  (caddr test)])
    (printf "test ~s:~s ..." test-id expr)
    (flush-output-port)
    (case type
     [(string) (test-with-string-output test-id expr out)]
     [else (error 'test (format "invalid test type ~s" type))])
    (printf " ok\n")))
 
(define (test-all)
  (let f ([i 0] [ls (reverse all-tests)])
    (if (null? ls)
        (printf "passed all ~s tests\n" i)
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


(define input-filter 
  (make-parameter (lambda (x) x)
    (lambda (x)
      (unless (procedure? x)
        (error 'input-filter (format "not a procedure ~s" x)))
      x)))

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
        (error 'runtime-file (format "not a string ~s" fname)))
      fname)))

(define compile-port
  (make-parameter
    (current-output-port)
    (lambda (p)
       (unless (output-port? p) 
         (error 'compile-port (format "not an output port ~s" p)))
       p)))

(define show-compiler-output (make-parameter #f))

(define (run-compile expr)
  (let ([p (open-output-file "stst.s" 'replace)])
    (parameterize ([compile-port p])
       (compile-program expr))
    (close-output-port p)))

(define (execute)
  (unless (fxzero? (system "./stst > stst.out"))
    (error 'execute "produced program exited abnormally")))

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

(define (test-with-string-output test-id expr expected-output)
   (run-compile expr)
   (build)
   (execute)
   (unless (string=? expected-output (get-string))
     (error 'test (format "output mismatch for test ~s, expected ~s, got ~s"
        test-id expected-output (get-string)))))

(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))

(define (compile-library)
  (let ([p (open-output-file (lib-file) 'replace)])
    (parameterize ([compile-port p])
      (emit-library))
    (close-output-port p)))