(load "tests-driver.scm")
;;(load "tests-1.2-req.scm")
(load "tests-1.1-req.scm")

(define fxshift     2)
(define fxmask   #x03)
(define bool_f   #x2F)
(define bool_t   #x6F)
(define wordsize    4) ; bytes

(define fixnum-bits (- (* wordsize 8) fxshift))

(define fxlower (- (expt 2 (- fixnum-bits 1))))

(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x))) ;;(boolean? x) ---))

(define (immediate-rep x)
  (cond
   [(fixnum? x) (ash x fxshift)]
   [else #f])) ;;---))

(define (emit-program x)
  (unless (immediate? x) (error 'emit-program (format "'~s' not an immediate" x)))
  (emit "  .text")
  (emit "  .globl scheme_entry")
  (emit "  .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "  movl $~s, %eax" (immediate-rep x))
  (emit "  ret"))
