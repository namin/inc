(add-tests-with-string-output "procedures"
  [(letrec () 12) => 12]
  [(letrec () (let ([x 5]) (fx+ x x))) => 10]
  [(letrec ([f (lambda () 5)]) 7) => 7]
  [(letrec ([f (lambda () 5)]) (let ([x 12]) x)) => 12]
  [(letrec ([f (lambda () 5)]) (f)) => 5]
  [(letrec ([f (lambda () 5)]) (let ([x (f)]) x)) => 5]
  [(letrec ([f (lambda () 5)]) (fx+ (f) 6)) => 11]
  [(letrec ([f (lambda () 5)]) (fx- 20 (f))) => 15]
  [(letrec ([f (lambda () 5)]) (fx+ (f) (f))) => 10]
  [(letrec ([f (lambda () (fx+ 5 7))]
            [g (lambda () 13)])
    (fx+ (f) (g))) => 25]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f 13)) => 25]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f (f 10))) => 34]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f (f (f 0)))) => 36]
  [(letrec ([f (lambda (x y) (fx+ x y))]
            [g (lambda (x) (fx+ x 12))])
    (f 16 (f (g 0) (fx+ 1 (g 0))))) => 41]
  [(letrec ([f (lambda (x) (g x x))]
            [g (lambda (x y) (fx+ x y))])
     (f 12)) => 24]
  [(letrec ([f (lambda (x)
                 (if (fxzero? x)
                     1
                     (fx* x (f (fx-1 x)))))])
      (f 5)) => 120]
  [(letrec ([e (lambda (x) (if (fxzero? x) #t (o (fx-1 x))))]
            [o (lambda (x) (if (fxzero? x) #f (e (fx-1 x))))])
     (e 25)) => #f])

;; (add-tests-with-string-output "deeply nested procedures"
;;   [(letrec ([sum (lambda (n ac)
;;                    (if (fxzero? n)
;;                         ac
;;                         (app sum (fx-1 n) (fx+ n ac))))])
;;     (app sum 10000 0)) => "50005000"]
;;   [(letrec ([e (lambda (x) (if (fxzero? x) #t (app o (fx-1 x))))]
;;             [o (lambda (x) (if (fxzero? x) #f (app e (fx-1 x))))])
;;      (app e 5000000)) => "#t"]
;; )
