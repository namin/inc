(add-tests "cons"
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
  [(let ([x (let ([y (+ 1 2)]) (* y y))])
     (cons x (+ x x)))
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
