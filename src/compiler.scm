;; An incremental compiler

(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")

;; Preamble

(define wordsize            8)
(define boolshift           6)
(define bool-f     #b00101111)
(define bool-t     #b01101111)
(define boolmask   #b00111111)
(define booltag    #b00101111)
(define charmask   #b00111111)
(define charshift           8)
(define chartag    #b00001111)
(define fxmask     #b00000011)
(define fxshift             2)
(define fxtag               0)
(define list-nil   #b00111111)

;; Range for fixnums
(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

(define (immediate-rep x)
  (cond
   [(fixnum? x) (ash x fxshift)]
   [(boolean? x) (if x bool-t bool-f)]
   [(null? x) list-nil]
   [(char? x) (bitwise-ior (ash (char->integer x) charshift) chartag)]
   [else #f]))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

;; Primitives with some magic global variables and macros
(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
                 (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
                 (lambda (arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error "Primitive Emitter" "Nope")))

;; Codegen helpers

(define (emit-label label)
  (emit "~a:" label))

(define (emit-function-header f)
  (emit "    .text")
  (emit "    .globl ~a" f)
  (emit "    .type ~a, @function" f)
  (emit-label f))

(define (emit-immediate x)
  (emit "    movl $~a, %eax" (immediate-rep x)))

(define (emit-primcall expr)
  (let ([prim (car expr)]
        [args (cdr expr)])
    ;; (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(define (emit-expr expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(primcall? expr) (emit-primcall expr)]
   [else (error "Emit expr" "Unknown form")]))

(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit-expr expr)
  (emit "    ret"))

(define compile-program emit-program)

;; A tiny stdlib
(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "    and $~s, %eax" boolmask)
  (emit "    cmp $~s, %eax" booltag)
  (emit-cmp-bool))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit "    and $~s, %eax" charmask)
  (emit "    cmp $~s, %eax" chartag)
  (emit-cmp-bool))

(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "    and $~s, %eax" fxmask)
  (emit "    cmp $~s, %eax" fxtag)
  (emit-cmp-bool))

(define-primitive ($fxzero? arg)
  (emit-expr arg)
  ;; Compare the entire register to fxtag
  (emit "    cmp $~s, %eax" fxtag)
  (emit-cmp-bool))

(define-primitive (null? arg)
  (emit-expr arg)
  ;; Compare the entire register to list-nil
  (emit "    cmp $~s, %eax" list-nil)
  (emit-cmp-bool))

(define-primitive (not arg)
  (emit-expr arg)
  (emit "  cmp $~s, %al" bool-f)
  (emit-cmp-bool))

(define-primitive ($fxadd1 arg)
  (emit-expr arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(define-primitive ($fxsub1 arg)
  (emit-expr arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))

;; The shift arithmetic left (SAL) and shift logical left (SHL) instructions
;; perform the same operation; they shift the bits in the destination operand to
;; the left (toward more significant bit locations). For each shift count, the
;; most significant bit of the destination operand is shifted into the CF flag,
;; and the least significant bit is cleared
(define-primitive ($fixnum->char arg)
  (emit-expr arg)
  (emit "    shll $~s, %eax" (- charshift fxshift))
  (emit "    orl $~s, %eax" chartag))

(define-primitive ($char->fixnum arg)
  (emit-expr arg)
  (emit "    shrl $~s, %eax" (- charshift fxshift))
  (emit "    orl $~s, %eax" fxtag))

(define (emit-cmp-bool)
  ;; SETE sets the destination operand to 0 or 1 depending on the settings of
  ;; the status flags (CF, SF, OF, ZF, and PF) in the EFLAGS register.
  (emit "    sete %al")
  ;; MOVZ copies the contents of the source operand (register or memory
  ;; location) to the destination operand (register) and zero extends the value.
  (emit "    movzb %al, %eax")
  (emit "    sal $~s, %al" boolshift)
  (emit "    or $~s, %al" bool-f))
