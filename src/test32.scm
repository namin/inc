(define fact-code
  '(letrec ([f (lambda (x) 
                 (if (fxzero? x)
                     1
                     (fx* x (f (fxsub1 x)))))])
    (f 5)))


(define (build32)
  (run-compile-32 fact-code)
  (build))