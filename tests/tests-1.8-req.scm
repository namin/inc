(add-tests-with-string-output "cons"
  [(fx+1 0) => 1]
  [(pair? (cons 1 2)) => #t]
  [(pair? 12) => #f]
  [(pair? #t) => #f]
  [(pair? #f) => #f]
  [(pair? ()) => #f]
  [(fixnum? (cons 12 43)) => #f]
  [(boolean? (cons 12 43)) => #f]
  [(null? (cons 12 43)) => #f]
  [(not (cons 12 43)) => #f]
  [(if (cons 12 43) 32 43) => 32]
  [(car (cons 1 23)) => 1]
  [(cdr (cons 43 123)) => 123]
  [(car (car (cons (cons 12 3) (cons #t #f)))) => 12]
  [(cdr (car (cons (cons 12 3) (cons #t #f)))) => 3]
  [(car (cdr (cons (cons 12 3) (cons #t #f)))) => #t]
  [(cdr (cdr (cons (cons 12 3) (cons #t #f)))) => #f]
  [(let ([x (let ([y (fx+ 1 2)]) (fx* y y))])
     (cons x (fx+ x x)))
   => (9 . 18)]
  [(let ([t0 (cons 1 2)] [t1 (cons 3 4)])
     (let ([a0 (car t0)] [a1 (car t1)] [d0 (cdr t0)] [d1 (cdr t1)])
       (let ([t0 (cons a0 d1)] [t1 (cons a1 d0)])
         (cons t0 t1))))
   => ((1 . 4) 3 . 2)]
  [(let ([t (cons 1 2)])
     (let ([t t])
       (let ([t t])
         (let ([t t])
           t))))
   => (1 . 2)]
  [(let ([t (let ([t (let ([t (let ([t (cons 1 2)]) t)]) t)]) t)]) t)
   => (1 . 2)]
  [(let ([x ()])
     (let ([x (cons x x)])
       (let ([x (cons x x)])
         (let ([x (cons x x)])
           (cons x x)))))
   => ((((()) ()) (()) ()) ((()) ()) (()) ())]
  [(cons (let ([x #t]) (let ([y (cons x x)]) (cons x y)))
         (cons (let ([x #f]) (let ([y (cons x x)]) (cons y x)))
               ()))
   => ((#t #t . #t) ((#f . #f) . #f))])


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
     (e 25)) => #f]
)


#!eof
(add-tests-with-string-output "deeply nested procedures"
  [(letrec ([sum (lambda (n ac)
                   (if (fxzero? n)
                        ac
                        (app sum (fx-1 n) (fx+ n ac))))])
    (app sum 10000 0)) => "50005000"]
  [(letrec ([e (lambda (x) (if (fxzero? x) #t (app o (fx-1 x))))]
            [o (lambda (x) (if (fxzero? x) #f (app e (fx-1 x))))])
     (app e 5000000)) => "#t"]
)
