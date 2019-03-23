#lang racket

(require "../src/driver.rkt")

(add-tests "procedures"
  [(letrec () 12) => 12]
  [(letrec () (let ([x 5]) (+ x x))) => 10]
  [(letrec ([f (lambda () 5)]) 7) => 7]
  [(letrec ([f (lambda () 5)]) (let ([x 12]) x)) => 12]
  [(letrec ([f (lambda () 5)]) (f)) => 5]
  [(letrec ([f (lambda () 5)]) (let ([x (f)]) x)) => 5]
  [(letrec ([f (lambda () 5)]) (+ (f) 6)) => 11]
  [(letrec ([f (lambda () 5)]) (- 20 (f))) => 15]
  [(letrec ([f (lambda () 5)]) (+ (f) (f))) => 10]
  [(letrec ([f (lambda () (+ 5 7))]
            [g (lambda () 13)])
    (+ (f) (g))) => 25]
  [(letrec ([f (lambda (x) (+ x 12))]) (f 13)) => 25]
  [(letrec ([f (lambda (x) (+ x 12))]) (f (f 10))) => 34]
  [(letrec ([f (lambda (x) (+ x 12))]) (f (f (f 0)))) => 36]
  [(letrec ([f (lambda (x y) (+ x y))]
            [g (lambda (x) (+ x 12))])
    (f 16 (f (g 0) (+ 1 (g 0))))) => 41]
  [(letrec ([f (lambda (x) (g x x))]
            [g (lambda (x y) (+ x y))])
     (f 12)) => 24]
  [(letrec ([f (lambda (x)
                 (if (zero? x)
                     1
                     (* x (f (dec x)))))])
      (f 5)) => 120]
  [(letrec ([e (lambda (x) (if (zero? x) #t (o (dec x))))]
            [o (lambda (x) (if (zero? x) #f (e (dec x))))])
     (e 25)) => #f])

;; (add-tests "deeply nested procedures"
;;   [(letrec ([sum (lambda (n ac)
;;                    (if (zero? n)
;;                         ac
;;                         (app sum (dec n) (+ n ac))))])
;;     (app sum 10000 0)) => "50005000"]
;;   [(letrec ([e (lambda (x) (if (zero? x) #t (app o (dec x))))]
;;             [o (lambda (x) (if (zero? x) #f (app e (dec x))))])
;;      (app e 5000000)) => "#t"]
;; )
