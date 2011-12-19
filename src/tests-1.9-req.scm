(add-tests-with-string-output "more stack"
  [(letrec ([f (lambda (n)
                 (if (fxzero? n)
                     0
                     (fx+ 1 (f (fxsub1 n)))))])
     (f 100000)) => "100000\n"])

(add-tests-with-string-output "more vectors"
  [(letrec ([f (lambda (v i)
                 (if (fx>= i 0)
                     (begin (vector-set! v i i) (f v (fxsub1 i)))
                     v))])
     (let ((v (make-vector 100)))
       (vector-length (f v 100)))) => "100\n"])

(load "tests-1.9.3-req.scm")
(load "tests-1.9.2-req.scm")
(load "tests-1.9.1-req.scm")
