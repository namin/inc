(add-tests-with-string-output "let loop"
  [(let ()
     (let loop ((x 1) (r 0))
       (if (fx= x 0)
           r
           (loop (fx- x 1) (fx+ r 1))))
   )
   => "1\n"]
)
