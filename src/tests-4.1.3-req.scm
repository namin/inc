(add-tests-with-string-output-noboot "write/display"
  [(fx+ -536870911 -1) => "-536870912\n"]
  [(begin
     (write '(1 2 3))
     (exit)) => "(1 2 3)"]
  [(begin
     (write '"Hello World!")
     (exit)) => "\"Hello World!\""]
)
