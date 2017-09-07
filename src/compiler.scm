;; An incremental compiler

(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")

;; Preamble

(define wordsize            8)
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

;; Range for fixnums
(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define (immediate-rep x)
  (cond
   [(fixnum? x) (ash x fxshift)]
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
  (unless (immediate? x) (error "" "Not an immediate value"))
  (emit-function-header "scheme_entry")
  (emit "    movl $~a, %eax" (immediate-rep x))
  (emit "    ret"))
