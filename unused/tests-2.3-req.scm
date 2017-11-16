
(add-tests-with-string-output "complex constants"
 ['42 => "42"]
 ['(1 . 2) => "(1 . 2)"]
 ['(1 2 3) => "(1 2 3)"]
 [(let ([x '(1 2 3)]) x) => "(1 2 3)"]
 [(let ([f (lambda () '(1 2 3))])
   (f)) => "(1 2 3)"]
 [(let ([f (lambda () '(1 2 3))])
   (eq? (f) (f))) => "#t"]
 [(let ([f (lambda ()
             (lambda ()
               '(1 2 3)))])
   ((f))) => "(1 2 3)"]
 [(let ([x '#(1 2 3)])
    (cons x (vector-ref x 0))) => "(#(1 2 3) . 1)"]
 ["Hello World" => "\"Hello World\""]
 ['("Hello" "World") => "(\"Hello\" \"World\")"]
)
