(add-tests-with-string-output "define function"
  [(let ()
     (define (x) 3)
     (define (y) 4)
     (fx+ (x) (y)))
   => "7\n"]
  [(let ()
     (define (f x y) (fx+ x y))
     (f 3 4))
   => "7\n"]
  [(let ()
     (define (f x) (fx+ x x))
     (f 3))
   => "6\n"])
