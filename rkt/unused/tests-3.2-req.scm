
(add-tests-with-string-output "error"
  [(error 'foo "here") => ""])


(add-tests-with-string-output "apply error"
  [(let ([f 6])
     (f f)) => ""]
  [(let ([f 6])
     (f (f))) => ""]
  [(1 2 3) => ""]
  [(1 (3 4)) => ""]
  [(let ([f (lambda () (1 2 3))])
     12) => "12"]
)

(add-tests-with-string-output "arg-check for fixed-arg procedures"
 [(let ([f (lambda () 12)])
    (f)) => "12"]
 [(let ([f (lambda () 12)])
    (f 1)) => ""]
 [(let ([f (lambda () 12)])
    (f 1 2)) => ""]
 [(let ([f (lambda (x) (fx+ x x))])
    (f)) => ""]
 [(let ([f (lambda (x) (fx+ x x))])
    (f 1)) => "2"]
 [(let ([f (lambda (x) (fx+ x x))])
    (f 1 2)) => ""]
 [(let ([f (lambda (x y) (fx* x (fx+ y y)))])
    (f)) => ""]
 [(let ([f (lambda (x y) (fx* x (fx+ y y)))])
    (f 2)) => ""]
 [(let ([f (lambda (x y) (fx* x (fx+ y y)))])
    (f 2 3)) => "12"]
 [(let ([f (lambda (x y) (fx* x (fx+ y y)))])
    (f 2 3 4)) => ""]
)

(add-tests-with-string-output "arg-check for var-arg procedures"
 [(let ([f (lambda x x)])
    (f)) => "()"]
 [(let ([f (lambda x x)])
    (f 'a)) => "(a)"]
 [(let ([f (lambda x x)])
    (f 'a 'b)) => "(a b)"]
 [(let ([f (lambda x x)])
    (f 'a 'b 'c)) => "(a b c)"]
 [(let ([f (lambda x x)])
    (f 'a 'b 'c 'd)) => "(a b c d)"]

 [(let ([f (lambda (x . rest) (vector x rest))])
    (f)) => ""]
 [(let ([f (lambda (x . rest) (vector x rest))])
    (f 'a)) => "#(a ())"]
 [(let ([f (lambda (x . rest) (vector x rest))])
    (f 'a 'b)) => "#(a (b))"]
 [(let ([f (lambda (x . rest) (vector x rest))])
    (f 'a 'b 'c)) => "#(a (b c))"]
 [(let ([f (lambda (x . rest) (vector x rest))])
    (f 'a 'b 'c 'd)) => "#(a (b c d))"]

 [(let ([f (lambda (x y . rest) (vector x y rest))])
    (f)) => ""]
 [(let ([f (lambda (x y . rest) (vector x y rest))])
    (f 'a)) => ""]
 [(let ([f (lambda (x y . rest) (vector x y rest))])
    (f 'a 'b)) => "#(a b ())"]
 [(let ([f (lambda (x y . rest) (vector x y rest))])
    (f 'a 'b 'c)) => "#(a b (c))"]
 [(let ([f (lambda (x y . rest) (vector x y rest))])
    (f 'a 'b 'c 'd)) => "#(a b (c d))"]
)


;;; (add-tests-with-string-output "arg-check for primitives"
;;;   [(cons 1 2 3) => ""]
;;;   [(cons 1) => ""]
;;;   [(vector-ref '#() 1 2 3 4) => ""]
;;;   [(vector-ref) => ""]
;;;   [(vector) => "#()"]
;;;   [(string) => "\"\""]
;;; )
