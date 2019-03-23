
(add-tests-with-string-output "nontail apply"
 [(let ([f (lambda () 12)])
   (fx+ (apply f '()) 1)) => "13"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (fx+ (apply f 13 '()) 1)) => "26"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (fx+ (apply f (cons 13 '())) 1)) => "26"]
 [(let ([f (lambda (x y z) (fx+ x (fx* y z)))])
   (fx+ (apply f 12 '(7 2)) 1)) => "27"]
 [(cons (apply vector '(1 2 3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 '(2 3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 '(3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 '(4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 '(5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 5 '(6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 5 6 '(7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 5 6 7 '(8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 5 6 7 8 ()) '()) => "(#(1 2 3 4 5 6 7 8))"]
)

(add-tests-with-string-output "tail apply"
 [(let ([f (lambda () 12)])
   (apply f '())) => "12"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (apply f 13 '())) => "25"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (apply f (cons 13 '()))) => "25"]
 [(let ([f (lambda (x y z) (fx+ x (fx* y z)))])
   (apply f 12 '(7 2))) => "26"]
 [(apply vector '(1 2 3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 '(2 3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 '(3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 '(4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 '(5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 5 '(6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 5 6 '(7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 5 6 7 '(8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 5 6 7 8 ()) => "#(1 2 3 4 5 6 7 8)"]
)




(add-tests-with-string-output "nontail apply"
 [(let ([f (lambda () 12)])
   (fx+ (apply f '()) 1)) => "13"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (fx+ (apply f 13 '()) 1)) => "26"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (fx+ (apply f (cons 13 '())) 1)) => "26"]
 [(let ([f (lambda (x y z) (fx+ x (fx* y z)))])
   (fx+ (apply f 12 '(7 2)) 1)) => "27"]
 [(cons (apply vector '(1 2 3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 '(2 3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 '(3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 '(4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 '(5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 5 '(6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 5 6 '(7 8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 5 6 7 '(8)) '()) => "(#(1 2 3 4 5 6 7 8))"]
 [(cons (apply vector 1 2 3 4 5 6 7 8 ()) '()) => "(#(1 2 3 4 5 6 7 8))"]
)

(add-tests-with-string-output "tail apply"
 [(let ([f (lambda () 12)])
   (apply f '())) => "12"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (apply f 13 '())) => "25"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (apply f (cons 13 '()))) => "25"]
 [(let ([f (lambda (x y z) (fx+ x (fx* y z)))])
   (apply f 12 '(7 2))) => "26"]
 [(apply vector '(1 2 3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 '(2 3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 '(3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 '(4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 '(5 6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 5 '(6 7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 5 6 '(7 8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 5 6 7 '(8)) => "#(1 2 3 4 5 6 7 8)"]
 [(apply vector 1 2 3 4 5 6 7 8 ()) => "#(1 2 3 4 5 6 7 8)"]
)
