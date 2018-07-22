#lang racket

(require "../src/compiler.rkt")

(provide all-tests add-tests test-all)

(define file-asm "/tmp/inc.s")
(define file-bin "inc")
(define file-out "/tmp/inc.out")

;; Collect all tests in a global variable
(define all-tests '())

;; Indirect way to get around mutating module level variables.
;;
;; tldr; `set!` is not allowed on module level variables, this function prevents
;; the error `set!: cannot mutate module-required identifier`
;; https://docs.racket-lang.org/guide/module-set.html
(define (mut! val)
  (set! all-tests val))

(define-syntax add-tests
  (syntax-rules (=> >>)
    [(_ test-name [input x expectation] ...)
     (mut! (cons '(test-name [input x expectation] ...) all-tests))]))

;; Run the compiler but send the output to a file instead
(define (compile-program expr)
  ;; Delete previous output file if any; existence of this file fails the test
  ;; consistently on docker.
  (system (format "rm -f ~a ~a" file-out file-bin))
  (let ([p (open-output-file file-asm #:exists 'replace)])
    (parameterize ([compile-port p])
      (emit-program expr))
    (close-output-port p)))

(define (build)
  (unless (system "make -C .. --quiet")
    (error 'make "Could not build target")))

;; Execute the binary and return the output as a string
(define (execute)
  (let ([command (format "./~a > ~a" file-bin file-out)])
    (unless (system command)
      (error 'make "Produced program exited abnormally"))
    (read (open-input-file file-out))))

;; Compile, build, execute and show the result in shell. Great for devel
(define (run expr)
  (compile-program expr)
  (build)
  (execute))

(define (test-one test-id test)
  (let ([input (car test)]
        [type (cadr test)]
        [expectation (caddr test)])
    (printf "Test ~s: ~s ..." test-id input)

    (case type

      ;; Compile, build, execute and assert output with expectation
      ['=> (let ([result (run input)])
            (unless (equal? expectation result)
              (error 'match-test
                     (format "~s. expected ~s, got ~s"
                             test-id expectation result))))]

      ;; Run a unit test
      ['>> (let* (;; Eval + namespace is really ugly and needs to be sorted out.
                  [ns (module->namespace "src/compiler.rkt")]
                  [result (eval input ns)])
            (unless (equal? result (eval expectation ns))
              (error 'unit-test
                     (format "~s. expected ~s, got ~s"
                             test-id (eval expectation ns) result))))])
    (printf " ok\n")))

(define (test-all)
  (let f ([i 0] [ls all-tests])
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
