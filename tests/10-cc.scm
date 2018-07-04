;; Closure conversion tests.
;; Note that these are unit tests (>> instead of usual =>)

(define sample '(letrec ([e (lambda (x) (if (zero? x) #t (o (dec x))))]
                         [o (lambda (x) (if (zero? x) #f (e (dec x))))])
                  (e 25)))

(define result '(letrec ([e (code
                             (x)
                             (if zero? dec o)
                             (if (zero? x) #t (o (dec x))))]
                         [o (code
                             (x)
                             (if zero? e dec)
                             (if (zero? x) #f (e (dec x))))])
                  (e 25)))


(add-tests "closure conversion"
  [(cc sample default-env) >> result]
  )
