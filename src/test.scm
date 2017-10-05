;; Test the compiler

;; I need to load the compiler file twice for some weird reason. I get this
;; silly error otherwise.
;;
;;     Performing cons tests ...
;;     test 0:(fx+1 0) ...
;;     Exception in compile-program: (fx+1 0) is not a string

(load "compiler.scm")
(load "compiler.scm")
(test-all)
