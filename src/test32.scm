(define fact-code
  '(letrec ([f (lambda (x) 
                 (if (fxzero? x)
                     1
                     (fx* x (f (fxsub1 x)))))])
    (f 5)))

(define cons-code
  '(pair? (cons (cons 12 3) (cons #t #f))))

(define (build32 code)
  (run-compile-32 code)
  (build))