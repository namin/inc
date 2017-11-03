
(add-tests-with-string-output "set!"
  [(let ([x 12])
     (set! x 13)
     x) => "13"]
  [(let ([x 12])
     (set! x (fx+1 x))
     x) => "13"]
  [(let ([x 12])
     (let ([x #f]) (set! x 14))
     x) => "12"]
  [(let ([x 12])
     (let ([y (let ([x #f]) (set! x 14))])
       x)) => "12"]
  [(let ([f #f])
     (let ([g (lambda () f)])
       (set! f 10)
       (g))) => "10"]
  [(let ([f (lambda (x)
              (set! x (fx+1 x))
              x)])
     (f 12)) => "13"]
  [(let ([x 10])
     (let ([f (lambda (x)
                (set! x (fx+1 x))
                x)])
       (cons x (f x)))) => "(10 . 11)"]
  [(let ([t #f])
     (let ([locative
          (cons
             (lambda () t)
             (lambda (n) (set! t n)))])
       ((cdr locative) 17)
       ((car locative)))) => "17"]
  [(let ([locative
          (let ([t #f])
            (cons
              (lambda () t)
              (lambda (n) (set! t n))))])
      ((cdr locative) 17)
      ((car locative))) => "17"]
  [(let ([make-counter
          (lambda ()
            (let ([counter -1])
              (lambda ()
                (set! counter (fx+1 counter))
                counter)))])
     (let ([c0 (make-counter)]
           [c1 (make-counter)])
       (c0)
       (cons (c0) (c1)))) => "(1 . 0)"]
  [(let ([fact #f])
     (set! fact (lambda (n)
                  (if (fxzero? n)
                      1
                      (fx* n (fact (fx-1 n))))))
     (fact 5)) => "120"]
  [(let ([fact #f])
     ((begin
         (set! fact (lambda (n)
                      (if (fxzero? n)
                          1
                          (fx* n (fact (fx-1 n))))))
         fact)
      5)) => "120"]

)
