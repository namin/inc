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
