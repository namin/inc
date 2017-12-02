;; An incremental compiler

(load "tests-driver.scm")

(load "../tests/01-integers.scm")
(load "../tests/02-immediate.scm")
(load "../tests/03-unary.scm")
(load "../tests/04-binop.scm")
(load "../tests/05-if.scm")
(load "../tests/06-let.scm")
(load "../tests/07-cons.scm")
(load "../tests/08-procedures.scm")

;; Constants
(define wordsize             8)

;; Stack index points to the current available empty slot. Use and then
;; decrement the index to add a new variable.
(define default-stack-index -8)

;; Constants for runtime representation
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
(define lammask    #b00000111)
(define lamtag     #b00000110)
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

;; Operations on alists

;; Add a key value pair to an alist; check if this exists in stdlib
(define (extend key value alist)
  (cons (list key value) alist))

;; Get a value from an alist; check if this exists in stdlib
(define (lookup key alist)
  (cond
   [(assv key alist) => cadr]
   [else #f]))

;; Primitives with some magic global variables and macros.

;; Top level environment
(define prim-env '())

(define-record-type primitive (fields name arity body))

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name arg* ...) b b* ...)
     (let ([p (make-primitive 'prim-name
                              (length '(arg* ...))
                              (lambda (arg* ...) b b* ...))])
       (set! prim-env (extend 'prim-name p prim-env)))]))

(define (primcall? expr)
  (and (pair? expr) (lookup (car expr) prim-env) #t))

(define (primitive-emitter name)
  (define (compose f g) (lambda (x) (f (g x))))
  (cond
   [(assv name prim-env) => (compose primitive-body cadr)]
   [else (error 'primitive-emitter (format "~a is not a primitive" name))]))

;; Environment with alist

;; An environment is an alist - list of pairs where car is the variable name and
;; cdr is the offset from the base pointer. Arguments to a function will be
;; positive and above the base pointer while local variables will be below. RBP
;; itself should point to the address to jump back to.
;;
;; Example: `((arg1 8) (b -24) (a -16))`
(define default-env '())

(define (tagged-list expr name)
  (and (list? expr) (eq? name (car expr)) #t))

(define (let? expr)
  (tagged-list expr 'let))

(define (bindings expr)
  (assert (let? expr))
  (cadr expr))

(define (body expr)
  (assert (let? expr))
  (cddr expr))

(define (if? expr)
  (tagged-list expr 'if))

(define (letrec? expr)
  (and (pair? expr) (eq? 'letrec (car expr))))

(define (variable? x env)
  (and (symbol? x) (lookup x env)))

;; Lambda; arguments at cadr, body at cddr
(define (lambda? expr)
  (tagged-list expr 'lambda ))

(define (app? env expr)
  (and (list? expr) (not (null? expr)) (lookup (car expr) env)))

;; Codegen helpers

(define (emit-label label)
  (emit "~a:" label))

(define (emit-program-header)
  (emit "    .intel_syntax noprefix")
  (emit "    .text"))

(define (emit-function-header f)
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

;; Claim or reclaim stack space by adjusting the stack pointer.
(define (emit-adjust-base si)
  (unless (= si 0) (emit "    add rsp, ~s" si)))

;; Extract the function body from primitive definition and call it
(define (emit-primcall si env expr)
  (let ([name (car expr)]
        [args (cdr expr)])
    ;; (check-primcall-args prim args)
    (apply (primitive-emitter name) (cons si (cons env args)))))

;; Fetch a variable from stack and load it into RAX
(define (emit-variable si env expr)
  (emit-stack-load (lookup expr env)))

;; Emit code for a let expression
;;
;; NOTE: All the space allocated by the let expression for local variables can
;; be freed at the end of the body. This implies the `si` stays the same before
;; and after a let expression. There is no need to keep track of the amount of
;; space allocated inside the let expression and free it afterwards.
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
    ;; TODO: Generate labels similar to prefix
    (lambda prefix
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

;; Top level definitions
(define (emit-letrec si env expr)
  (letrec ([first car]
           [second cadr]
           [make-env (lambda (lvars labels)
                       (map list lvars labels))]
           ;; TODO: Allow multiple expressions in letrec body
           [letrec-body caddr])
    (let* ([bindings (cadr expr)]
           [lvars (map first bindings)]
           [lambdas (map second bindings)]
           ;; [labels (map unique-label lvars)]
           [env (append env (make-env lvars lvars))])
      (for-each (emit-lambda env) lambdas lvars)
      (emit-entry si env (letrec-body expr)))))

;; Function body for the simplest C style functions
;;
;; A lot of required sanity and safety checks are missing.
;;
;; The calling convention expected by the function is kind of odd and needs to
;; be standardized. Arguments are pushed to stack in order (unlike cdecl, which
;; pushes in reverse order).
;;
;; The caller of the function emits arguments at `RSP - 24`, then `RSP - 32`
;; etc. The function preamble effectively decrements the base pointer by 0x10
;; such that the such that the first argument can be accessed at `RBP - 8`, the
;; next one at `RBP - 16` etc.
;;
(define (emit-lambda env)
  (lambda (expr label)
    (emit-function-header label)
    (emit-preamble)
    (let ([formals (cadr expr)]
          ;; TODO: Emit code for all expressions, not just car
          [body (car (cddr expr))])
      (let f ([formals formals]
              ;; The first argument to the function is available at `RSP - 8`
              [si (- wordsize)]
              [env env])
        (if (null? formals)
            (emit-expr si env body)
            (f (cdr formals)
               (- si wordsize)
               (extend (car formals) si env)))))
    (emit-ret)))

(define (emit-app si env expr)
  (define (emit-arguments si args)
    (unless (null? args)
      (emit-expr si env (car args))
      (emit-stack-save si)
      (emit-arguments (- si wordsize) (cdr args))))

  (let* ([name (lookup (car expr) env)]
         [args (cdr expr)]
         [arity (length args)])

    ;; Evaluate and push the arguments into stack; 2 words below SI. See
    ;; `emit-lambda` docs for a detailed description of how this works.
    ;;
    ;; si  832 -> ...
    ;;     816 -> ...
    ;;     808 -> arg 1
    ;;     800 -> arg 2
    (emit-arguments (- si (* 2 wordsize)) args)

    ;; Extend stack to hold the current local variables before creating a new
    ;; frame for the function call.
    (emit-adjust-base (+ si wordsize))

    (emit-call name)

    ;; NOTE: This is one of those big aha moments. There is no need to reclaim
    ;; space used for function arguments because the memory would just get
    ;; overridden by next variable allocation. This makes keeping track of that
    ;; stack index unnecessary and life so much simpler.
    (emit-adjust-base (- (+ si wordsize)))))

(define (emit-expr si env expr)
  (cond
   [(immediate? expr) (emit-immediate si env expr)]
   [(primcall? expr) (emit-primcall si env expr)]
   [(variable? expr env) (emit-variable si env expr)]
   [(let? expr) (emit-let si env (bindings expr) (body expr))]
   [(if? expr) (emit-if si env (cadr expr) (caddr expr) (cadddr expr))]
   [(lambda? expr) (emit-lambda si env (cadr expr) (caddr expr))]
   [(app? env expr) (emit-app si env expr)]
   [else (error 'emit-expr (format "Unknown form ~a" expr))]))

;; Emit the program entry point, this should be called just once
(define (emit-entry si env expr)
  (emit "    .globl init")
  (emit-function-header "init")
  (emit-preamble)
  (emit-heap-init)
  (emit-expr si env expr)
  (emit-ret))

;; Entry point of the compiler - principal API
(define (emit-program expr)
  (emit-program-header)
  (let ([si default-stack-index]
        [env default-env])
    (cond
     [(letrec? expr) (emit-letrec si env expr)]
     [else (emit-entry si env expr)])))

;; ASM wrappers

;; Unconditional function call
(define (emit-call label)
  (emit "    call ~a" label))

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
   [(> si 0) (format "[rbp + ~s]" si)]
   [(< si 0) (format "[rbp - ~s]" (- si))]))

;; Save the variable in register RAX to stack index SI
(define (emit-stack-save si)
  (emit "    mov ~a, rax" (get-stack-ea si)))

;; Load variable from stack into register RAX
(define (emit-stack-load si)
  (emit "    mov rax, ~a" (get-stack-ea si)))

;; Contradicts with `emit-stack-save`; both in the same program feels inelegant.
(define (emit-push value)
  (emit "    push ~a" value))

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
