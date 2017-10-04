;; An incremental compiler

(load "tests-driver.scm")

(load "tests-1.8-req.scm")
(load "tests-1.7-req.scm")
(load "tests-1.6-req.scm")
(load "tests-1.5-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.1-req.scm")

;; Preamble

(define wordsize            8)
(define bool-f     #b00101111)
(define bool-t     #b01101111)
(define boolmask   #b00111111)
(define boolshift           6)
(define booltag    #b00101111)
(define charmask   #b00111111)
(define charshift           8)
(define chartag    #b00001111)
(define fxmask     #b00000011)
(define fxshift             2)
(define fxtag               0)
(define heapmask   #b00000111)
(define list-nil   #b00111111)
(define pairtag    #b00000001)

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
   [else (error 'immediate-rep (format "Unknown form ~a" x))]))

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
  (or (getprop x '*emitter*) (error 'primitive-emitter "Nope")))

(define (let? expr)
  (eq? 'let (car expr)))

(define (bindings expr)
  (assert (let? expr))
  (cadr expr))

(define (body expr)
  (assert (let? expr))
  (cddr expr))

(define (if? expr)
  (eq? 'if (car expr)))

;; Env with alist. It's shitty that we use both alist and plist
(define (extend var si env)
  (cons (list var si) env))

(define (lookup var env)
  (cond
   [(assv var env) => cadr]
   [else #f]))

(define (variable? x env)
  (and (symbol? x) (lookup x env)))

;; Codegen helpers

(define (emit-label label)
  (emit "~a:" label))

(define (emit-function-header f)
  (emit "    .intel_syntax noprefix")
  (emit "    .text")
  (emit "    .globl ~a" f)
  (emit "    .type ~a, @function" f)
  (emit-label f))

(define (emit-immediate si env x)
  (emit "    mov rax, ~a" (immediate-rep x)))

;; The function preamble, emitted before every function entry
(define (emit-preamble)
  (emit "    push rbp")
  (emit "    mov rbp, rsp"))

;; Function exit
(define (emit-ret)
  (emit "    pop rbp")
  (emit "    ret"))

;; Extract the function body from primitive definition and call it
(define (emit-primcall si env expr)
  (let ([prim (car expr)]
        [args (cdr expr)])
    ;; (check-primcall-args prim args)
    (apply (primitive-emitter prim) (cons si (cons env args)))))

;; Fetch a variable from stack and load it into RAX
(define (emit-variable si env expr)
  (emit-stack-load (lookup expr env)))

;; Emit code for a let expression
(define (emit-let si env bindings body)
  (let f ((si si) (new-env env) (b* bindings))
    (cond
     ;; TODO: (car body) is wrong, should support multiple expressions in body
     [(null? b*) (emit-expr si new-env (car body))]
     [else
      (let ([b (car b*)])
        (emit-expr si new-env (cadr b))
        (emit-stack-save si)
        (f (next-stack-index si)
           (extend (car b) si new-env)
           (cdr b*)))])))

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (string->symbol (format "L_~s" count))])
        (set! count (add1 count))
        L))))

(define (emit-if si env test conseq alt)
  (let ([alt-label (unique-label)]
        [final-label (unique-label)])
    (emit-expr si env test)
    (emit-cmp (immediate-rep #f))
    (emit-je alt-label)
    (emit-expr si env conseq)
    (emit-jmp final-label)
    (emit-label alt-label)
    (emit-expr si env alt)
    (emit-label final-label)))

(define (emit-expr si env expr)
  (cond
   [(immediate? expr) (emit-immediate si env expr)]
   [(primcall? expr) (emit-primcall si env expr)]
   [(variable? expr env) (emit-variable si env expr)]
   [(let? expr) (emit-let si env (bindings expr) (body expr))]
   [(if? expr) (emit-if si env (cadr expr) (caddr expr) (cadddr expr))]
   [else (error 'emit-expr (format "Unknown form ~a" (car expr)))]))

(define (emit-program expr)
  (let ([default-stack-index -8]
        [default-env '()])
    (emit-function-header "init")
    (emit-preamble)
    (emit-heap-init)
    (emit-expr default-stack-index default-env expr)
    (emit-ret)))

(define compile-program emit-program)

;; ASM wrappers

;; Compare the value to register RAX
(define (emit-cmp with)
  (emit "    cmp rax, ~a" with))

;; Jump to the specified label if last comparison resulted in equality
(define (emit-je label)
  (emit "    je ~a" label))

;; Unconditionally jump to the specified label
(define (emit-jmp label)
  (emit "    jmp ~a" label))

;; All the primitives; functions defined in asm
(define-primitive (boolean? si env expr)
  (emit-expr si env expr)
  (emit "    and rax, ~s" boolmask)
  (emit-cmp booltag)
  (emit-cmp-bool))

(define-primitive (char? si env expr)
  (emit-expr si env expr)
  (emit "    and rax, ~s" charmask)
  (emit-cmp chartag)
  (emit-cmp-bool))

(define-primitive (fixnum? si env expr)
  (emit-expr si env expr)
  (emit "    and rax, ~s" fxmask)
  (emit-cmp fxtag)
  (emit-cmp-bool))

(define-primitive (pair? si env expr)
  (emit-expr si env expr)
  (emit "    and rax, ~s" heapmask)
  (emit-cmp pairtag)
  (emit-cmp-bool))

(define-primitive (fxzero? si env expr)
  (emit-expr si env expr)
  (emit-cmp fxtag)
  (emit-cmp-bool))

(define-primitive (null? si env expr)
  (emit-expr si env expr)
  (emit-cmp list-nil)
  (emit-cmp-bool))

(define-primitive (not si env expr)
  (emit-expr si env expr)
  (emit "    cmp al, ~s" bool-f)
  (emit-cmp-bool))

(define-primitive (fx+1 si env expr)
  (emit-expr si env expr)
  (emit "    add rax, ~s" (immediate-rep 1)))

(define-primitive (fx-1 si env expr)
  (emit-expr si env expr)
  (emit "    sub rax, ~s" (immediate-rep 1)))

;; The shift arithmetic left (SAL) and shift logical left (SHL) instructions
;; perform the same operation; they shift the bits in the destination operand to
;; the left (toward more significant bit locations). For each shift count, the
;; most significant bit of the destination operand is shifted into the CF flag,
;; and the least significant bit is cleared
(define-primitive (fixnum->char si env expr)
  (emit-expr si env expr)
  (emit "    shl rax, ~s" (- charshift fxshift))
  (emit "    or rax, ~s" chartag))

(define-primitive (char->fixnum si env expr)
  (emit-expr si env expr)
  (emit "    shr rax, ~s" (- charshift fxshift))
  (emit "    or rax, ~s" fxtag))

(define-primitive (fxlognot si env expr)
  (emit-expr si env expr)
  (emit "  shr rax, ~s" fxshift)
  (emit "  not rax")
  (emit "  shl rax, ~s" fxshift))

(define-primitive (fxlogor si env expr1 arg2)
  (emit-binop si env expr1 arg2)
  (emit "  or rax, ~a" (get-stack-ea si)))

(define-primitive (fxlogand si env expr1 arg2)
  (emit-binop si env expr1 arg2)
  (emit "  and rax, ~a" (get-stack-ea si)))

(define (get-stack-ea si)
  (assert (not (= si 0)))
  (cond
   [(> si 0) (format "[rsp + ~s]" si)]
   [(< si 0) (format "[rsp - ~s]" (- si))]))

;; Save the variable in register RAX to stack index SI
(define (emit-stack-save si)
  (emit "    mov ~a, rax" (get-stack-ea si)))

;; Load variable from stack into register RAX
(define (emit-stack-load si)
  (emit "    mov rax, ~a" (get-stack-ea si)))

;; The base address of the heap is passed in RDI and we reserve reg RSI for it.
(define (emit-heap-init)
  (emit "    mov rsi, rdi    # Store heap index to RSI"))

(define (next-stack-index si)
  (- si wordsize))

(define (emit-binop si env a b)
  (emit-expr si env a)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env b))

(define-primitive (fx+ si env a b)
  (emit-binop si env a b)
  (emit "    add rax, ~a" (get-stack-ea si)))

(define-primitive (fx- si env a b)
  (emit-binop si env a b)
  ;; Subtracts the 2nd op from the first and stores the result in the 1st
  (emit "    sub ~a, rax" (get-stack-ea si))
  ;; This is pretty inefficient to update result in stack and load it back.
  ;; Reverse the order and fix it up.
  (emit-stack-load si))

(define-primitive (fx* si env a b)
  (emit-binop si env a b)
  (emit "    shr rax, ~s" fxshift)
  ;; The destination operand is an implied operand located in register AX
  ;; GCC throws `Error: ambiguous operand size for `mul'` without size
  ;; quantifier
  (emit "    mulq ~a" (get-stack-ea si)))

;; Division turned out to be much more trickier than I expected it to be. Unlike
;; @namin's code, I'm using a shift arithmetic right (SAR) instead of shift
;; logical right (SHR) and I don't know how the original examples worked at all
;; for negative numbers. I also had to use the CQO instruction to Sign-Extend
;; RAX which the 32 bit version is obviously not concerned with. I got the idea
;; from GCC disassembly.
;;
;; Dividend is passed in RDX:RAX and IDIV instruction takes the divisor as the
;; argument. the quotient is stored in RAX and the remainder in RDX.
(define (emit-div si env a b)
  (emit-expr si env b)
  (emit "    sar rax, ~s" fxshift)
  (emit "    mov rcx, rax")
  (emit-expr si env a)
  (emit "    sar rax, ~s" fxshift)
  (emit "    mov rdx, 0")
  (emit "    cqo")
  (emit "    idivq rcx    # ~a/~a" a b))

(define-primitive (fxquotient si env a b)
  (emit-div si env a b)
  (emit "    shl rax, ~s" fxshift))

(define-primitive (fxremainder si env a b)
  (emit-div si env a b)
  (emit "   mov rax, rdx")
  (emit "   shl rax, ~s" fxshift))

(define (emit-cmp-bool . args)
  ;; SETE sets the destination operand to 0 or 1 depending on the settings of
  ;; the status flags (CF, SF, OF, ZF, and PF) in the EFLAGS register.
  (emit "    ~s al" (if (null? args) 'sete (car args)))
  ;; MOVZX copies the contents of the source operand (register or memory
  ;; location) to the destination operand (register) and zero extends the value.
  (emit "    movzx rax, al")
  (emit "    sal al, ~s" boolshift)
  (emit "    or al, ~s" bool-f))

(define (emit-cmp-binop setx si env a b)
  (emit-binop si env a b)
  (emit "    cmp ~a, rax" (get-stack-ea si))
  (emit-cmp-bool setx))

(define-primitive (fx= si env a b)
  (emit-cmp-binop 'sete si env a b))

(define-primitive (fx< si env a b)
  (emit-cmp-binop 'setl si env a b))

(define-primitive (fx<= si env a b)
  (emit-cmp-binop 'setle si env a b))

(define-primitive (fx> si env a b)
  (emit-cmp-binop 'setg si env a b))

(define-primitive (fx>= si env a b)
  (emit-cmp-binop 'setge si env a b))

;; Allocate a pair in the heap
(define-primitive (cons si env a b)
  ;; Evaluate the arguments, push to stack for temp storage. This feels horribly
  ;; inefficient to write to ram and back.
  (emit-expr si env a)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env b)
  (emit-stack-save (next-stack-index si))

  (emit "    mov rax, ~a" (get-stack-ea si))
  (emit "    movq [rsi + 0], rax    # '(~a ...) "  a)
  (emit "    mov rax, ~a" (get-stack-ea (next-stack-index si)))
  (emit "    movq [rsi + 8], rax    # '(... ~a) "  b)
  (emit "    mov rax, rsi")
  (emit "    or rax, ~a" pairtag)
  (emit "    add rsi, ~a" (* 2 wordsize)))

(define-primitive (car si env pair)
  ;; assert destination is really a pair ?
  (emit-expr si env pair)
  (emit "    mov rax, [rax - 1]         # (car ~a) " pair))

(define-primitive (cdr si env pair)
  ;; assert destination is really a pair ?
  (emit-expr si env pair)
  (emit "    mov rax, [rax + 7]         # (car ~a) " pair))
