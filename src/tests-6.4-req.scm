(add-tests-with-string-output-noboot "load compiler"
  [(let ()
     (load "self.scm")
     (load "reader.scm")
     (load "compiler.scm")
     1
   )
   => "1\n"]
)
