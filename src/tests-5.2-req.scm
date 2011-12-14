

(add-tests-with-string-output "overflow"
  [(letrec ([f
     (lambda (i)
       (when (fx<= i 1000)
         (let ([x (make-list 1000)])
           (f (fxadd1 i)))))])
    (f 0)
    100) => "100\n"]
  [(letrec ([f
     (lambda (i)
       (when (fx<= i 100000)
         (let ([x (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)])
           (f (fxadd1 i)))))])
    (f 0)
    100) => "100\n"])
