
(add-tests-with-string-output "nontail apply"
 [(let ([f (lambda () 12)])
   (fx+ (apply f '()) 1)) => "13\n"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (fx+ (apply f 13 '()) 1)) => "26\n"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (fx+ (apply f (cons 13 '())) 1)) => "26\n"]
 [(let ([f (lambda (x y z) (fx+ x (fx* y z)))])
   (fx+ (apply f 12 '(7 2)) 1)) => "27\n"]
 [(cons (apply vector '(1 2 3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 '(2 3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 '(3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 '(4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 '(5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 5 '(6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 5 6 '(7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 5 6 7 '(8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 5 6 7 8 ()) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
)

(add-tests-with-string-output "tail apply"
 [(let ([f (lambda () 12)])
   (apply f '())) => "12\n"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (apply f 13 '())) => "25\n"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (apply f (cons 13 '()))) => "25\n"]
 [(let ([f (lambda (x y z) (fx+ x (fx* y z)))])
   (apply f 12 '(7 2))) => "26\n"]
 [(apply vector '(1 2 3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 '(2 3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 '(3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 '(4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 '(5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 5 '(6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 5 6 '(7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 5 6 7 '(8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 5 6 7 8 ()) => "#(1 2 3 4 5 6 7 8)\n"]
)




(add-tests-with-string-output "nontail apply"
 [(let ([f (lambda () 12)])
   (fx+ (apply f '()) 1)) => "13\n"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (fx+ (apply f 13 '()) 1)) => "26\n"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (fx+ (apply f (cons 13 '())) 1)) => "26\n"]
 [(let ([f (lambda (x y z) (fx+ x (fx* y z)))])
   (fx+ (apply f 12 '(7 2)) 1)) => "27\n"]
 [(cons (apply vector '(1 2 3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 '(2 3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 '(3 4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 '(4 5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 '(5 6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 5 '(6 7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 5 6 '(7 8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 5 6 7 '(8)) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
 [(cons (apply vector 1 2 3 4 5 6 7 8 ()) '()) => "(#(1 2 3 4 5 6 7 8))\n"]
)

(add-tests-with-string-output "tail apply"
 [(let ([f (lambda () 12)])
   (apply f '())) => "12\n"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (apply f 13 '())) => "25\n"]
 [(let ([f (lambda (x) (fx+ x 12))])
   (apply f (cons 13 '()))) => "25\n"]
 [(let ([f (lambda (x y z) (fx+ x (fx* y z)))])
   (apply f 12 '(7 2))) => "26\n"]
 [(apply vector '(1 2 3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 '(2 3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 '(3 4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 '(4 5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 '(5 6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 5 '(6 7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 5 6 '(7 8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 5 6 7 '(8)) => "#(1 2 3 4 5 6 7 8)\n"]
 [(apply vector 1 2 3 4 5 6 7 8 ()) => "#(1 2 3 4 5 6 7 8)\n"]
)
