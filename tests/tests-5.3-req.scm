

(add-tests-with-string-output "call/cc"
  [(call/cc (lambda (k) 12)) => "12\n"]
  [(call/cc (lambda (k) (k 12))) => "12\n"]
  [(call/cc (lambda (k) (fx+ 1 (k 12)))) => "12\n"]
  [(fx+ (call/cc (lambda (k) (k 12)))
        (call/cc (lambda (k) 13))) => "25\n"]
  [(letrec ([fact
             (lambda (n k)
               (cond
                 [(fxzero? n) (k 1)]
                 [else (fx* n (fact (fxsub1 n) k))]))])
     (call/cc
       (lambda (k)
         (fact 5 k)))) => "1\n"]
  [(call/cc
    (lambda (k)
      (letrec ([fact
                (lambda (n)
                  (cond
                    [(fxzero? n) (k 1)]
                    [else (fx* n (fact (fxsub1 n)))]))])
        (fact 5)))) => "1\n"]
  [(let ([k #f])
     (letrec ([fact
               (lambda (n)
                 (cond
                   [(fxzero? n) 
                    (call/cc
                      (lambda (nk)
                        (set! k nk)
                        (k 1)))]
                   [else (fx* n (fact (fxsub1 n)))]))])
        (let ([v (fact 5)])
          v))) => "120\n"]
  [(let ([k #f])
     (letrec ([fact
               (lambda (n)
                 (cond
                   [(fxzero? n) 
                    (call/cc
                      (lambda (nk)
                        (set! k nk)
                        (k 1)))]
                   [else (fx* n (fact (fxsub1 n)))]))])
        (let ([v (fact 5)])
          (let ([nk k])
            (set! k (lambda (x) (cons v x)))
            (nk v))))) => "(120 . 14400)\n"]
  )

