;; An incremental compiler

(load "tests-driver.scm")
(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.5-req.scm")

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
  (emit "    .intel_syntax noprefix")
  (emit "    .text")
  (emit "    .globl ~a" f)
  (emit "    .type ~a, @function" f)
  (emit-label f))

(define (emit-immediate si x)
  (emit "    mov rax, ~a" (immediate-rep x)))

(define (emit-primcall si expr)
  (let ([prim (car expr)]
        [args (cdr expr)])
    ;; (check-primcall-args prim args)
    (apply (primitive-emitter prim) (cons si args))))

(define (emit-expr si expr)
  (cond
   [(immediate? expr) (emit-immediate si expr)]
   [(primcall? expr) (emit-primcall si expr)]
   [else (error "Emit expr" "Unknown form")]))

(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit-expr -8 expr)
  (emit "    ret"))

(define compile-program emit-program)

;; A tiny stdlib
(define-primitive (boolean? si arg)
  (emit-expr si arg)
  (emit "    and rax, ~s" boolmask)
  (emit "    cmp rax, ~s" booltag)
  (emit-cmp-bool))

(define-primitive (char? si arg)
  (emit-expr si arg)
  (emit "    and rax, ~s" charmask)
  (emit "    cmp rax, ~s" chartag)
  (emit-cmp-bool))

(define-primitive (fixnum? si arg)
  (emit-expr si arg)
  (emit "    and rax, ~s" fxmask)
  (emit "    cmp rax, ~s" fxtag)
  (emit-cmp-bool))

(define-primitive ($fxzero? si arg)
  (emit-expr si arg)
  ;; Compare the entire register to fxtag
  (emit "    cmp rax, ~s" fxtag)
  (emit-cmp-bool))

(define-primitive (null? si arg)
  (emit-expr si arg)
  ;; Compare the entire register to list-nil
  (emit "    cmp rax, ~s" list-nil)
  (emit-cmp-bool))

(define-primitive (not si arg)
  (emit-expr si arg)
  (emit "  cmp al, ~s" bool-f)
  (emit-cmp-bool))

(define-primitive ($fxadd1 si arg)
  (emit-expr si arg)
  (emit "    add rax, ~s" (immediate-rep 1)))

(define-primitive ($fxsub1 si arg)
  (emit-expr si arg)
  (emit "    sub rax, ~s" (immediate-rep 1)))

;; The shift arithmetic left (SAL) and shift logical left (SHL) instructions
;; perform the same operation; they shift the bits in the destination operand to
;; the left (toward more significant bit locations). For each shift count, the
;; most significant bit of the destination operand is shifted into the CF flag,
;; and the least significant bit is cleared
(define-primitive ($fixnum->char si arg)
  (emit-expr si arg)
  (emit "    shl rax, ~s" (- charshift fxshift))
  (emit "    or rax, ~s" chartag))

(define-primitive ($char->fixnum si arg)
  (emit-expr si arg)
  (emit "    shr rax, ~s" (- charshift fxshift))
  (emit "    or rax, ~s" fxtag))

(define-primitive (fx+ si a b)
  (emit-expr si a)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) b)
  (emit "    add rax, ~a" (get-stack-ea si)))

(define (get-stack-ea si)
  (assert (< si 0))
  (format "[rsp - ~s]" (abs si)))

(define (emit-stack-save si)
  (assert (< si 0))
  (emit "    mov ~a, rax" (get-stack-ea si)))

(define (next-stack-index si)
  (- si wordsize))

(define (emit-cmp-bool)
  ;; SETE sets the destination operand to 0 or 1 depending on the settings of
  ;; the status flags (CF, SF, OF, ZF, and PF) in the EFLAGS register.
  (emit "    sete al")
  ;; MOVZX copies the contents of the source operand (register or memory
  ;; location) to the destination operand (register) and zero extends the value.
  (emit "    movzx rax, al")
  (emit "    sal al, ~s" boolshift)
  (emit "    or al, ~s" bool-f))
