
(add-tests-with-string-output "cons"
  [(fxadd1 0) => "1\n"]
  [(pair? (cons 1 2)) => "#t\n"]
  [(pair? 12) => "#f\n"]
  [(pair? #t) => "#f\n"]
  [(pair? #f) => "#f\n"]
  [(pair? ()) => "#f\n"]
  [(fixnum? (cons 12 43)) => "#f\n"]
  [(boolean? (cons 12 43)) => "#f\n"]
  [(null? (cons 12 43)) => "#f\n"]
  [(not (cons 12 43)) => "#f\n"]
  [(if (cons 12 43) 32 43) => "32\n"]
  [(car (cons 1 23)) => "1\n"]
  [(cdr (cons 43 123)) => "123\n"]
  [(car (car (cons (cons 12 3) (cons #t #f)))) => "12\n"]
  [(cdr (car (cons (cons 12 3) (cons #t #f)))) => "3\n"]
  [(car (cdr (cons (cons 12 3) (cons #t #f)))) => "#t\n"]
  [(cdr (cdr (cons (cons 12 3) (cons #t #f)))) => "#f\n"]
  [(let ([x (let ([y (fx+ 1 2)]) (fx* y y))])
     (cons x (fx+ x x)))
   => "(9 . 18)\n"]
  [(let ([t0 (cons 1 2)] [t1 (cons 3 4)])
     (let ([a0 (car t0)] [a1 (car t1)] [d0 (cdr t0)] [d1 (cdr t1)])
       (let ([t0 (cons a0 d1)] [t1 (cons a1 d0)])
         (cons t0 t1))))
   => "((1 . 4) 3 . 2)\n"]
  [(let ([t (cons 1 2)])
     (let ([t t])
       (let ([t t])
         (let ([t t])
           t))))
   => "(1 . 2)\n"]
  [(let ([t (let ([t (let ([t (let ([t (cons 1 2)]) t)]) t)]) t)]) t)
   => "(1 . 2)\n"]
  [(let ([x ()])
     (let ([x (cons x x)])
       (let ([x (cons x x)])
         (let ([x (cons x x)])
           (cons x x)))))
   => "((((()) ()) (()) ()) ((()) ()) (()) ())\n"]
  [(cons (let ([x #t]) (let ([y (cons x x)]) (cons x y)))
         (cons (let ([x #f]) (let ([y (cons x x)]) (cons y x))) 
               ())) 
   => "((#t #t . #t) ((#f . #f) . #f))\n"]
)

  

#!eof
(add-tests-with-string-output "procedures"
  [(letrec () 12) => "12\n"]
  [(letrec () (let ([x 5]) (fx+ x x))) => "10\n"]
  [(letrec ([f (lambda () 5)]) 7) => "7\n"]
  [(letrec ([f (lambda () 5)]) (let ([x 12]) x)) => "12\n"]
  [(letrec ([f (lambda () 5)]) (f)) => "5\n"]
  [(letrec ([f (lambda () 5)]) (let ([x (f)]) x)) => "5\n"]
  [(letrec ([f (lambda () 5)]) (fx+ (f) 6)) => "11\n"]
  [(letrec ([f (lambda () 5)]) (fx- 20 (f))) => "15\n"]
  [(letrec ([f (lambda () 5)]) (fx+ (f) (f))) => "10\n"]
  [(letrec ([f (lambda () (fx+ 5 7))]
            [g (lambda () 13)]) 
    (fx+ (f) (g))) => "25\n"]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f 13)) => "25\n"]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f (f 10))) => "34\n"]
  [(letrec ([f (lambda (x) (fx+ x 12))]) (f (f (f 0)))) => "36\n"]
  [(letrec ([f (lambda (x y) (fx+ x y))] 
            [g (lambda (x) (fx+ x 12))])
    (f 16 (f (g 0) (fx+ 1 (g 0))))) => "41\n"]
  [(letrec ([f (lambda (x) (g x x))]
            [g (lambda (x y) (fx+ x y))])
     (f 12)) => "24\n"]
  [(letrec ([f (lambda (x) 
                 (if (fxzero? x)
                     1
                     (fx* x (f (fxsub1 x)))))])
      (f 5)) => "120\n"]
  [(letrec ([e (lambda (x) (if (fxzero? x) #t (o (fxsub1 x))))]
            [o (lambda (x) (if (fxzero? x) #f (e (fxsub1 x))))])
     (e 25)) => "#f\n"]
)

(add-tests-with-string-output "deeply nested procedures"
  [(letrec ([sum (lambda (n ac)
                   (if (fxzero? n)
                        ac
                        (app sum (fxsub1 n) (fx+ n ac))))])
    (app sum 10000 0)) => "50005000\n"]
  [(letrec ([e (lambda (x) (if (fxzero? x) #t (app o (fxsub1 x))))]
            [o (lambda (x) (if (fxzero? x) #f (app e (fxsub1 x))))])
     (app e 5000000)) => "#t\n"]
)
