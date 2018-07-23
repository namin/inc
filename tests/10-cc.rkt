#lang racket

(require "../src/driver.rkt")

;; Closure conversion tests.
;; Note that these are unit tests (>> instead of usual =>)

(define sample '(letrec ([e (lambda (x) (if (zero? x) #t (o (dec x))))]
                         [o (lambda (x) (if (zero? x) #f (e (dec x))))])
                  (e 25)))

(define lifted '(labels ([o (code (x) (e) (if (zero? x) #f (e (dec x))))]
                         [e (code (x) (o) (if (zero? x) #t (o (dec x))))])
                  (labelcall e 25)))

(add-tests "free variables"

  ;; Don't blow up for the simplest cases
  [(free-vars '(+ x y) default-env) >> '(x y)]

  ;; Handle a lambda
  [(free-vars '(lambda (x) (+ x y)) default-env) >> '(y)]

  ;; Avoid duplicates
  [(free-vars '(lambda (z) (+ x x)) default-env) >> '(x)]

  ;; No false positives
  [(free-vars '(lambda (x) (+ x x)) default-env) >> '()]

  ;; Something non trivial
  ;; [(free-vars sample default-env) >> '(e o)]

  ;; Macros are indeed a fucking PITA. Variables like `sample` wont be expanded
  ;; to the value here because its not part of the lexical scope of the macros.
  ;; Hygiene is NOT what I want here. Inlining the variable for now.
  [(free-vars '(letrec ([e (lambda (x) (if (zero? x) #t (o (dec x))))]
                        [o (lambda (x) (if (zero? x) #f (e (dec x))))])
                 (e 25))
              default-env) >> '(e o)])

(add-tests "Closure conversion"

  ;; Simple recursive letrec
  [(lift empty '(letrec ([e (lambda (x) (if (zero? x) #t (o (dec x))))]
                         [o (lambda (x) (if (zero? x) #f (e (dec x))))])
                  (e 25)) ) >>
               '(labels ([o (code (x) (e) (if (zero? x) #f (e (dec x))))]
                         [e (code (x) (o) (if (zero? x) #t (o (dec x))))])
                        (labelcall e 25))])
