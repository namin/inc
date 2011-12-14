
(add-tests-with-string-output "complex constants"
 ['42 => "42\n"]
 ['(1 . 2) => "(1 . 2)\n"]
 ['(1 2 3) => "(1 2 3)\n"]
 [(let ([x '(1 2 3)]) x) => "(1 2 3)\n"]
 [(let ([f (lambda () '(1 2 3))])
   (f)) => "(1 2 3)\n"]
 [(let ([f (lambda () '(1 2 3))])
   (eq? (f) (f))) => "#t\n"]
 [(let ([f (lambda ()
             (lambda () 
               '(1 2 3)))])
   ((f))) => "(1 2 3)\n"]
 [(let ([x '#(1 2 3)]) 
    (cons x (vector-ref x 0))) => "(#(1 2 3) . 1)\n"]
 ["Hello World" => "\"Hello World\"\n"]
 ['("Hello" "World") => "(\"Hello\" \"World\")\n"]
)
