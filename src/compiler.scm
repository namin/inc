;; An incremental compiler

(load "tests-driver.scm")
(load "tests-1.1-req.scm")

;; Preamble

(define (emit-label label)
  (emit "~a:" label))

(define (emit-function-header f)
  (emit "  .text")
  (emit "  .globl ~a" f)
  (emit "  .type ~a, @function" f)
  (emit-label f))

(define (compile-program x)
  (emit-function-header "scheme_entry")
  (emit "    movl $~a, %eax" x)
  (emit "    ret"))
