(add-tests-with-string-output "vector"
 [(fx= 1 2) => "#f"]
 [(vector 1 2 3 4 5) => "#(1 2 3 4 5)"]
 [(let ([f (lambda (f) (f 1 2 3 4 5 6))])
   (f vector)) => "#(1 2 3 4 5 6)"]
 )
