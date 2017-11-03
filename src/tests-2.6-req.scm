; vararg tests


(add-tests-with-string-output "vararg not using rest argument"
  [(let ([f (lambda args 12)])
    (f)) => "12"]
  [(let ([f (lambda args 12)])
    (f 10)) => "12"]
  [(let ([f (lambda args 12)])
    (f 10 20)) => "12"]
  [(let ([f (lambda args 12)])
    (f 10 20 30)) => "12"]
  [(let ([f (lambda args 12)])
    (f 10 20 30 40)) => "12"]
  [(let ([f (lambda args 12)])
    (f 10 20 30 40 50)) => "12"]
  [(let ([f (lambda args 12)])
    (f 10 20 30 40 50 60 70 80 90)) => "12"]
  [(let ([f (lambda (a0 . args) 12)])
    (f 10)) => "12"]
  [(let ([f (lambda (a0 . args) a0)])
    (f 10)) => "10"]
  [(let ([f (lambda (a0 . args) 12)])
    (f 10 20)) => "12"]
  [(let ([f (lambda (a0 . args) a0)])
    (f 10 20)) => "10"]
  [(let ([f (lambda (a0 . args) 12)])
    (f 10 20 30)) => "12"]
  [(let ([f (lambda (a0 . args) a0)])
    (f 10 20 30)) => "10"]
  [(let ([f (lambda (a0 . args) 12)])
    (f 10 20 30 40)) => "12"]
  [(let ([f (lambda (a0 . args) a0)])
    (f 10 20 30 40)) => "10"]
  [(let ([f (lambda (a0 a1 . args) (vector a0 a1))])
    (f 10 20 30 40 50 60 70 80 90 100)) => "#(10 20)"]
  [(let ([f (lambda (a0 a1 a2 . args) (vector a0 a1 a2))])
    (f 10 20 30 40 50 60 70 80 90 100)) => "#(10 20 30)"]
  [(let ([f (lambda (a0 a1 a2 a3 . args) (vector a0 a1 a2 a3))])
    (f 10 20 30 40 50 60 70 80 90 100)) => "#(10 20 30 40)"]
  [(let ([f (lambda (a0 a1 a2 a3 a4 . args) (vector a0 a1 a2 a3 a4))])
    (f 10 20 30 40 50 60 70 80 90 100)) => "#(10 20 30 40 50)"]
  [(let ([f (lambda (a0 a1 a2 a3 a4 a5 . args) (vector a0 a1 a2 a3 a4 a5))])
    (f 10 20 30 40 50 60 70 80 90 100)) => "#(10 20 30 40 50 60)"]
)


(add-tests-with-string-output "vararg using rest argument"
  [(let ([f (lambda args args)])
    (f)) => "()"]
  [(let ([f (lambda args args)])
    (f 10)) => "(10)"]
  [(let ([f (lambda args args)])
    (f 10 20)) => "(10 20)"]
  [(let ([f (lambda args args)])
    (f 10 20 30)) => "(10 20 30)"]
  [(let ([f (lambda args args)])
    (f 10 20 30 40)) => "(10 20 30 40)"]
  [(let ([f (lambda (a0 . args) (vector a0 args))])
    (f 10)) => "#(10 ())"]
  [(let ([f (lambda (a0 . args) (vector a0 args))])
    (f 10 20)) => "#(10 (20))"]
  [(let ([f (lambda (a0 . args) (vector a0 args))])
    (f 10 20 30)) => "#(10 (20 30))"]
  [(let ([f (lambda (a0 . args) (vector a0 args))])
    (f 10 20 30 40)) => "#(10 (20 30 40))"]
  [(let ([f (lambda (a0 a1 . args) (vector a0 a1 args))])
    (f 10 20 30 40 50 60 70 80 90)) => "#(10 20 (30 40 50 60 70 80 90))"]
  [(let ([f (lambda (a0 a1 a2 . args) (vector a0 a1 a2 args))])
    (f 10 20 30 40 50 60 70 80 90)) => "#(10 20 30 (40 50 60 70 80 90))"]
  [(let ([f (lambda (a0 a1 a2 a3 . args) (vector a0 a1 a2 a3 args))])
    (f 10 20 30 40 50 60 70 80 90)) => "#(10 20 30 40 (50 60 70 80 90))"]
  [(let ([f (lambda (a0 a1 a2 a3 a4 . args) (vector a0 a1 a2 a3 a4 args))])
    (f 10 20 30 40 50 60 70 80 90)) => "#(10 20 30 40 50 (60 70 80 90))"]
  [(let ([f (lambda (a0 a1 a2 a3 a4 a5 . args)(vector a0 a1 a2 a3 a4 a5 args))])
    (f 10 20 30 40 50 60 70 80 90)) => "#(10 20 30 40 50 60 (70 80 90))"]
)
