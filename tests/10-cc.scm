;; Closure conversion tests.
;; Note that these are unit tests (>> instead of usual =>)

(define sample '(letrec ([e (lambda (x) (if (zero? x) #t (o (dec x))))]
                         [o (lambda (x) (if (zero? x) #f (e (dec x))))])
                  (e 25)))

(define result '(letrec ([e (code (x) (o) (if (zero? x) #t (o (dec x))))]
                         [o (code (x) (e) (if (zero? x) #f (e (dec x))))])
                  (e 25)))

(add-tests "free variables"

  ;; Don't blow up for the simplest cases
  ;; TODO: The order is messed up, but that's OK for now.
  [(free-vars '(+ x y) default-env) >> '(y x)]

  ;; Handle a lambda
  [(free-vars '(lambda (x) (+ x y)) default-env) >> '(y)]

  ;; Avoid duplicates
  [(free-vars '(lambda (z) (+ x x)) default-env) >> '(x)]

  ;; No false positives
  [(free-vars '(lambda (x) (+ x x)) default-env) >> '()]

  ;; Something non trivial
  [(free-vars sample default-env) >> '(e o)])

(add-tests "closure conversion"

  ;; The simplest example
  [(cc '(lambda (x) (+ x y)) default-env) >>
       '(code (x) (y) (+ x y))]

  [(cc sample default-env) >> result])
