(add-tests-with-string-output "system"
  [(let ()
     (system "echo hello")
   )
   => "hello\n0\n"]
)
