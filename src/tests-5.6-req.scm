
(add-tests-with-string-output "fxmodulo"
  [(fxmodulo  16  4) => "0\n"]
  [(fxmodulo   5  2) => "1\n"]
  [(fxmodulo -45  7) => "4\n"]
  [(fxmodulo  10 -3) => "-2\n"]
  [(fxmodulo -17 -9) => "-8\n"]
  
  [(let ([t  4]) (fxmodulo  16 t)) => "0\n"]
  [(let ([t  2]) (fxmodulo   5 t)) => "1\n"]
  [(let ([t  7]) (fxmodulo -45 t)) => "4\n"]
  [(let ([t -3]) (fxmodulo  10 t)) => "-2\n"]
  [(let ([t -9]) (fxmodulo -17 t)) => "-8\n"]
) 

