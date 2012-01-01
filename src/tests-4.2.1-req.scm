(add-tests-with-string-output "inner define"
  [(let ()
     (define x 3)
     (define y 4)
     (fx+ x y))
   => "7\n"]
  [(let ()
     (define x 3)
     (set! x 4)
     (define y x)
     (fx+ x y))
   => "8\n"])