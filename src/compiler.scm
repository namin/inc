;; An incremental compiler

(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")

;; Preamble

(define bool-f     #b00101111)
(define bool-t     #b01101111)
(define boolshift           7)
(define booltag    #b00011111)
(define charmask   #b00111111)
(define charshift           8)
(define chartag    #b00001111)
(define fxshift             2)
(define fxtag               0)
(define list-nil   #b00111111)

(define (immediate-rep x)
  (cond
   [(integer? x) (ash x fxshift)]
   [(boolean? x) (if x bool-t bool-f)]
   [(null? x) list-nil]
   [(char? x) (bitwise-ior (ash (char->integer x) charshift) chartag)]
   [else #f]))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-function-header f)
  (emit "  .text")
  (emit "  .globl ~a" f)
  (emit "  .type ~a, @function" f)
  (emit-label f))

(define (compile-program x)
  (emit-function-header "scheme_entry")
  (emit "    movl $~a, %eax" (immediate-rep x))
  (emit "    ret"))
