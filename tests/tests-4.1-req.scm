(add-tests-with-string-output "remainder/modulo/quotient"
  [#\tab => "#\\tab\n"]
  [(fxquotient 16 4) => "4\n"]
  [(fxquotient 5 2) => "2\n"]
  [(fxquotient -45 7) => "-6\n"]
  [(fxquotient 10 -3) => "-3\n"]
  [(fxquotient -17 -9) => "1\n"]

  [(fxremainder 16 4) => "0\n"]
  [(fxremainder 5 2) => "1\n"]
  [(fxremainder -45 7) => "-3\n"]
  [(fxremainder 10 -3) => "1\n"]
  [(fxremainder -17 -9) => "-8\n"] 

;  [(fxmodulo 16 4) => "0\n"]
;  [(fxmodulo 5 2) => "1\n"]
;  [(fxmodulo -45 7) => "4\n"]
;  [(fxmodulo 10 -3) => "-2\n"]
;  [(fxmodulo -17 -9) => "-8\n"]
)   

(add-tests-with-string-output "write-char"
  [(begin 
    (write-char #\a)
    (flush-output-port (current-output-port))
    (exit)) => "a"]
  [(begin 
    (write-char #\a)
    (close-output-port (current-output-port))
    (exit)) => "a"]
  [(begin 
    (write-char #\H)
    (write-char #\e)
    (write-char #\l)
    (write-char #\l)
    (write-char #\o)
    (write-char #\space)
    (flush-output-port)
    (write-char #\W)
    (write-char #\o)
    (write-char #\r)
    (write-char #\l)
    (write-char #\d)
    (write-char #\!)
    (flush-output-port (current-output-port))
    (exit)) => "Hello World!"]
)


(add-tests-with-string-output "write/display"
  [(fx+ -536870911 -1) => "-536870912\n"]
  [(begin
     (write '(1 2 3))
     (exit)) => "(1 2 3)"]
  [(begin
     (write '"Hello World!")
     (exit)) => "\"Hello World!\""]
)
