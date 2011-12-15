(load "tests-driver.scm")
;(load "tests-1.5-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.1-req.scm")

(define fxshift        2)
(define fxmask      #x03)
(define fxtag       #x00)
(define bool-f      #x2F)
(define bool-t      #x6F)
(define bool-bit       6)
(define boolmask    #xBF)
(define list-nil    #x3F)
(define charshift      8)
(define charmask    #x3F)
(define chartag     #x0F)
(define wordsize       4) ; bytes

(define fixnum-bits (- (* wordsize 8) fxshift))

(define fxlower (- (expt 2 (- fixnum-bits 1))))

(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (null? x) (char? x)))

(define (immediate-rep x)
  (cond
   [(fixnum? x) (ash x fxshift)]
   [(boolean? x) (if x bool-t bool-f)]
   [(null? x) list-nil]
   [(char? x) (bitwise-ior (ash (char->integer x) charshift) chartag)]
   [else #f]))

(define (emit-immediate x)
  (emit "  movl $~s, %eax" (immediate-rep x)))

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
         (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
         (lambda (si arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'primitive-emitter (format "primitive ~s has no emitter" x))))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (= (getprop prim '*arg-count*) (length args)))

(define (emit-primcall si expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si args)))

(define-primitive ($fxadd1 si arg)
  (emit-expr si arg)
  (emit "  addl $~s, %eax" (immediate-rep 1)))

(define-primitive ($fxsub1 si arg)
  (emit-expr si arg)
  (emit "  subl $~s, %eax" (immediate-rep 1)))

(define-primitive ($fixnum->char si arg)
  (emit-expr si arg)
  (emit "  shll $~s, %eax" (- charshift fxshift))
  (emit "  orl $~s, %eax" chartag))

(define-primitive ($char->fixnum si arg)
  (emit-expr si arg)
  (emit "  shrl $~s, %eax" (- charshift fxshift)))

(define-primitive (fixnum? si arg)
  (emit-expr si arg)
  (emit "  and $~s, %al" fxmask)
  (emit "  cmp $~s, %al" fxtag)
  (emit-cmp-bool))

(define (emit-cmp-bool)
  (emit "  sete %al")
  (emit "  movzbl %al, %eax")
  (emit "  sal $~s, %al" bool-bit)
  (emit "  or $~s, %al" bool-f))

(define-primitive ($fxzero? si arg)
  (emit-expr si arg)
  (emit "  cmp $~s, %al" fxtag)
  (emit-cmp-bool))

(define-primitive (null? si arg)
  (emit-expr si arg)
  (emit "  cmp $~s, %al" list-nil)
  (emit-cmp-bool))

(define-primitive (boolean? si arg)
  (emit-expr si arg)
  (emit "  and $~s, %al" boolmask)
  (emit "  cmp $~s, %al" bool-f)
  (emit-cmp-bool))

(define-primitive (char? si arg)
  (emit-expr si arg)
  (emit "  and $~s, %al" charmask)
  (emit "  cmp $~s, %al" chartag)
  (emit-cmp-bool))

(define-primitive (not si arg)
  (emit-expr si arg)
  (emit "  cmp $~s, %al" bool-f)
  (emit-cmp-bool))

(define-primitive ($fxlognot si arg)
  (emit-expr si arg)
  (emit "  shr $~s, %eax" fxshift)
  (emit "  not %eax")
  (emit "  shl $~s, %eax" fxshift))

(define-primitive (fx+ si arg1 arg2)
  (emit-expr si arg1)
  (emit "  movl %eax, ~s(%rsp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "  addl ~s(%rsp), %eax" si))

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        L))))

(define (if? expr)
  (and (list? expr) (eq? (car expr) 'if) (= 3 (length (cdr expr)))))
(define if-test cadr)
(define if-conseq caddr)
(define if-altern cadddr)

(define (emit-if si expr)
  (let ([alt-label (unique-label)]
        [end-label (unique-label)])
    (emit-expr si (if-test expr))
    (emit "  cmp $~s, %al" bool-f)
    (emit "  je ~a" alt-label)
    (emit-expr si (if-conseq expr))
    (emit "  jmp ~a" end-label)
    (emit-label alt-label)
    (emit-expr si (if-altern expr))
    (emit-label end-label)))

(define (emit-expr si expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(primcall? expr) (emit-primcall si expr)]
   [(if? expr) (emit-if si expr)]
   [else (error 'emit-expr (format "~s is not an expression" expr))]))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-function-header f)
  (emit "  .text")
  (emit "  .globl ~a" f)
  (emit "  .type ~a, @function" f)
  (emit-label f))

(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit "  movq %rsp, %rcx")
  (emit "  movq 8(%rsp), %rsp")
  (emit "  call L_scheme_entry")
  (emit "  movq %rcx, %rsp")
  (emit "  ret")
  (emit-label "L_scheme_entry")
  (emit-expr (- wordsize) expr)
  (emit "  ret"))
