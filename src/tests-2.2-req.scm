
(add-tests-with-string-output "set!"
  [(let ([x 12])
     (set! x 13)
     x) => "13\n"]
  [(let ([x 12])
     (set! x (fxadd1 x))
     x) => "13\n"]
  [(let ([x 12])
     (let ([x #f]) (set! x 14))
     x) => "12\n"]
  [(let ([x 12])
     (let ([y (let ([x #f]) (set! x 14))])
       x)) => "12\n"]
  [(let ([f #f])
     (let ([g (lambda () f)])
       (set! f 10)
       (g))) => "10\n"]
  [(let ([f (lambda (x) 
              (set! x (fxadd1 x))
              x)])
     (f 12)) => "13\n"]
  [(let ([x 10])
     (let ([f (lambda (x) 
                (set! x (fxadd1 x))
                x)])
       (cons x (f x)))) => "(10 . 11)\n"]
  [(let ([t #f])
     (let ([locative
          (cons
             (lambda () t)
             (lambda (n) (set! t n)))])
       ((cdr locative) 17)
       ((car locative)))) => "17\n"]
  [(let ([locative
          (let ([t #f])
            (cons
              (lambda () t)
              (lambda (n) (set! t n))))])
      ((cdr locative) 17)
      ((car locative))) => "17\n"]
  [(let ([make-counter
          (lambda ()
            (let ([counter -1])
              (lambda ()
                (set! counter (fxadd1 counter))
                counter)))])
     (let ([c0 (make-counter)]
           [c1 (make-counter)])
       (c0)
       (cons (c0) (c1)))) => "(1 . 0)\n"]
  [(let ([fact #f])
     (set! fact (lambda (n)
                  (if (fxzero? n)
                      1
                      (fx* n (fact (fxsub1 n))))))
     (fact 5)) => "120\n"]
  [(let ([fact #f])
     ((begin 
         (set! fact (lambda (n)
                      (if (fxzero? n)
                          1
                          (fx* n (fact (fxsub1 n))))))
         fact)
      5)) => "120\n"]
  
) 

