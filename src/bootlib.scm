(load "compiler.scm")

(compile-program
 '(let ()
    (load "self.scm")
    (load "reader.scm")
    (load "compiler.scm")
    (parameterize ([lib-file "bootlib.s"])
      (compile-lib))))

