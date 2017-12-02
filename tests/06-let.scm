(add-tests "let"
  [(let ([x 5]) x) => 5]
  [(let ([x (fx+ 1 2)]) x) => 3]
  [(let ([x (fx+ 1 2)])
     (let ([y (fx+ 3 4)])
       (fx+ x y)))
   => 10]
  [(let ([x (fx+ 1 2)])
     (let ([y (fx+ 3 4)])
       (fx- y x)))
   => 4]
  [(let ([x (fx+ 1 2)]
         [y (fx+ 3 4)])
     (fx- y x))
   => 4]
  [(let ([x (let ([y (fx+ 1 2)]) (fx* y y))])
     (fx+ x x))
   => 18]
  [(let ([x (fx+ 1 2)])
     (let ([x (fx+ 3 4)])
       x))
   => 7]
  [(let ([x (fx+ 1 2)])
     (let ([x (fx+ x 4)])
       x))
   => 7]
  [(let ([t (let ([t (let ([t (let ([t (fx+ 1 2)]) t)]) t)]) t)]) t)
   => 3]
  [(let ([x 12])
     (let ([x (fx+ x x)])
       (let ([x (fx+ x x)])
         (let ([x (fx+ x x)])
           (fx+ x x)))))
   => 192]
)
