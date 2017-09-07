
(add-tests-with-string-output "exit"
 [(foreign-call "exit" 0) => ""]
)

(add-tests-with-string-output "S_error"
 [(let ([error (lambda args
                 (foreign-call "ik_error" args))])
   (error #f "died")
   12) => ""]

 [(let ([error (lambda args
                 (foreign-call "ik_error" args))])
   (error 'car "died")
   12) => ""]
)
