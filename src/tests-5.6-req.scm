(add-tests-with-string-output "fxmodulo"
  [(fxmodulo  16  4) => "0"]
  [(fxmodulo   5  2) => "1"]
  [(fxmodulo -45  7) => "4"]
  [(fxmodulo  10 -3) => "-2"]
  [(fxmodulo -17 -9) => "-8"]

  [(let ([t  4]) (fxmodulo  16 t)) => "0"]
  [(let ([t  2]) (fxmodulo   5 t)) => "1"]
  [(let ([t  7]) (fxmodulo -45 t)) => "4"]
  [(let ([t -3]) (fxmodulo  10 t)) => "-2"]
  [(let ([t -9]) (fxmodulo -17 t)) => "-8"]
)
