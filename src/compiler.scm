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
(load "../tests/09-strings.scm")

;; Constants
(define wordsize             8)

;; Stack index points to the current available empty slot. Use and then
;; decrement the index to add a new variable.
(define default-stack-index -8)

;; Constants for runtime representation
;;
;; Immediate values (values that can be fit in one machine word) are tagged for
;; distinguising them from heap allocated pointers. The last 3 bits effectively
;; serve as the runtime type of the value. Always using 3 bits is a simpler
;; approach than the multi bit technique the paper uses. This is a very
;; efficient and low overhead technique at the cost of losing precision -
;; completely acceptable for types like characters and booleans but having to
;; live with 61bit numerics instead of native 64 and some overhead for
;; operations like multiplication & division.
;;
(define fxtag   0) ;; 0 makes most arithmetic quite efficient
(define booltag 1)
(define chartag 2)
(define pairtag 3)
(define niltag  4)
(define strtag  5)
(define heaptag 7)

;; Value to bitwise or with a word to get the tag
(define mask #b111)
;; Number of bits a word must be shifted right to get its true value.
(define shift 3)

;; Boolean values can be computed just once upfront
(define bool-f (bitwise-ior (ash 0 shift) booltag))
(define bool-t (bitwise-ior (ash 1 shift) booltag))

;; Range for fixnums
(define fixnum-bits (- (* wordsize 8) shift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))

(define (immediate-rep x)
  (cond
   [(fixnum? x) (ash x shift)]
   [(boolean? x) (if x bool-t bool-f)]
   [(null? x) niltag]
   [(char? x) (bitwise-ior (ash (char->integer x) shift) chartag)]
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

;; Emit code for a function application. See `emit-lambda` for details.
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
    ;; frame for the function call. `si` is the next available empty slot, `(+
    ;; si wordsize)` is the current usage. Add this to `RSP` to reserve this
    ;; space before the function gets called. Not doing this will result in the
    ;; called function to override this space with its local variables and
    ;; corrupt the stack.
    (emit-adjust-base (+ si wordsize))

    (emit-call name)

    ;; NOTE: This is one of those big aha moments. There is no need to reclaim
    ;; space used for function arguments because the memory would just get
    ;; overridden by next variable allocation. This makes keeping track of that
    ;; stack index unnecessary and life so much simpler.
    (emit-adjust-base (- (+ si wordsize)))))

;; Allocate a string in heap
;;
;; Unlike null terminated C strings, we use length prefixed range of bytes. Each
;; character needs to be immediate encoded before its written to stack.
;;
;; 1. Look for asm instructions that can do the bytes in bulk rather than one at
;; a time. Even if its not any more efficient, the generated asm should be a lot
;; easier to read.
;;
;; 2. This is technically a ByteString rather than Text. We wont be able to
;; handle multi byte encoding like UTF-8 with this approach.
;;
(define (emit-string si env str)

  (let* ([len (string-length str)]
         [size (* wordsize (+ 1 len))])

    (emit "    mov qword ptr [rsi + 0], ~a    # Store \"~a\"" len str)

    (let f ([index wordsize]
            [ls (string->list str)])

      (unless (null? ls)
        (emit "    mov qword ptr [rsi + ~a], ~a" index (immediate-rep (car ls)))
        (f (+ wordsize index) (cdr ls))))

    (emit "    mov rax, rsi")
    (emit "    or rax, ~a" strtag)
    (emit "    add rsi, ~a" size)))

(define (emit-expr si env expr)
  (cond
   [(immediate? expr) (emit-immediate si env expr)]
   [(primcall? expr) (emit-primcall si env expr)]
   [(variable? expr env) (emit-variable si env expr)]
   [(string? expr) (emit-string si env expr)]
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
(define-primitive (fixnum? si env expr)
  (emit-expr si env expr)
  (emit "    and rax, ~s" mask)
  (emit-cmp fxtag)
  (emit-cmp-bool))

(define-primitive (boolean? si env expr)
  (emit-expr si env expr)
  (emit "    and rax, ~s" mask)
  (emit-cmp booltag)
  (emit-cmp-bool))

(define-primitive (char? si env expr)
  (emit-expr si env expr)
  (emit "    and rax, ~s" mask)
  (emit-cmp chartag)
  (emit-cmp-bool))

(define-primitive (pair? si env expr)
  (emit-expr si env expr)
  (emit "    and rax, ~s" mask)
  (emit-cmp pairtag)
  (emit-cmp-bool))

(define-primitive (null? si env expr)
  (emit-expr si env expr)
  (emit-cmp niltag)
  (emit-cmp-bool))

(define-primitive (string? si env expr)
  (emit-expr si env expr)
  (emit "    and rax, ~s" mask)
  (emit-cmp strtag)
  (emit-cmp-bool))

(define-primitive (zero? si env expr)
  (emit-expr si env expr)
  (emit-cmp fxtag)
  (emit-cmp-bool))

(define-primitive (not si env expr)
  (emit-expr si env expr)
  (emit "    cmp al, ~s" bool-f)
  (emit-cmp-bool))

(define-primitive (inc si env expr)
  (emit-expr si env expr)
  (emit "    add rax, ~s" (immediate-rep 1)))

(define-primitive (dec si env expr)
  (emit-expr si env expr)
  (emit "    sub rax, ~s" (immediate-rep 1)))

;; The shift arithmetic left (SAL) and shift logical left (SHL) instructions
;; perform the same operation; they shift the bits in the destination operand to
;; the left (toward more significant bit locations). For each shift count, the
;; most significant bit of the destination operand is shifted into the CF flag,
;; and the least significant bit is cleared
(define-primitive (fixnum->char si env expr)
  (emit-expr si env expr)
  (emit "    or rax, ~s" chartag))

(define-primitive (char->fixnum si env expr)
  (emit-expr si env expr)
  (emit "    sub rax, ~s" chartag)
  (emit "    or rax, ~s" fxtag))

(define-primitive (lognot si env expr)
  (emit-expr si env expr)
  (emit "    shr rax, ~s" shift)
  (emit "    not rax")
  (emit "    shl rax, ~s" shift))

(define-primitive (logor si env a b)
  (emit-binop si env a b)
  (emit "    or rax, ~a" (get-stack-ea si)))

(define-primitive (logand si env a b)
  (emit-binop si env a b)
  (emit "    and rax, ~a" (get-stack-ea si)))

(define-primitive (make-string si env len)
  (let ([size (* wordsize (+ 1 len))])
    (emit "    mov qword ptr [rsi + 0], ~a    # (make-string ~a)" len len)
    (emit "    mov rax, rsi")
    (emit "    or rax, ~a" strtag)
    (emit "    add rsi, ~a" size)))

(define-primitive (string-length si env str)
  (emit-expr si env str)
  (emit "    mov rax, qword ptr [rax - ~a]" strtag)
  (emit "    shl rax, ~s" shift))

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

(define-primitive (+ si env a b)
  (emit-binop si env a b)
  (emit "    add rax, ~a" (get-stack-ea si)))

(define-primitive (- si env a b)
  (emit-binop si env a b)
  ;; Subtracts the 2nd op from the first and stores the result in the 1st
  (emit "    sub ~a, rax" (get-stack-ea si))
  ;; This is pretty inefficient to update result in stack and load it back.
  ;; Reverse the order and fix it up.
  (emit-stack-load si))

(define-primitive (* si env a b)
  (emit-binop si env a b)
  (emit "    shr rax, ~s" shift)
  ;; The destination operand is an implied operand located in register AX
  ;; GCC throws `Error: ambiguous operand size for `mul'` without size
  ;; quantifier
  (emit "    mul qword ptr ~a" (get-stack-ea si)))

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
  (emit "    sar rax, ~s" shift)
  (emit "    mov rcx, rax")
  (emit-expr si env a)
  (emit "    sar rax, ~s" shift)
  (emit "    mov rdx, 0")
  (emit "    cqo")
  (emit "    idiv rcx    # ~a/~a" a b))

(define-primitive (quotient si env a b)
  (emit-div si env a b)
  (emit "    shl rax, ~s" shift))

(define-primitive (remainder si env a b)
  (emit-div si env a b)
  (emit "    mov rax, rdx")
  (emit "    shl rax, ~s" shift))

(define (emit-cmp-bool . args)
  ;; SETE sets the destination operand to 0 or 1 depending on the settings of
  ;; the status flags (CF, SF, OF, ZF, and PF) in the EFLAGS register.
  (emit "    ~s al" (if (null? args) 'sete (car args)))
  ;; MOVZX copies the contents of the source operand (register or memory
  ;; location) to the destination operand (register) and zero extends the value.
  (emit "    movzx rax, al")
  (emit "    sal al, ~s" shift)
  (emit "    or al, ~s" bool-f))

(define (emit-cmp-binop setx si env a b)
  (emit-binop si env a b)
  (emit "    cmp ~a, rax" (get-stack-ea si))
  (emit-cmp-bool setx))

(define-primitive (= si env a b)
  (emit-cmp-binop 'sete si env a b))

(define-primitive (< si env a b)
  (emit-cmp-binop 'setl si env a b))

(define-primitive (<= si env a b)
  (emit-cmp-binop 'setle si env a b))

(define-primitive (> si env a b)
  (emit-cmp-binop 'setg si env a b))

(define-primitive (>= si env a b)
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
  (emit "    mov qword ptr [rsi + 0], rax    # '(~a ...)"  a)
  (emit "    mov rax, ~a" (get-stack-ea (next-stack-index si)))
  (emit "    mov qword ptr [rsi + 8], rax    # '(... ~a)"  b)
  (emit "    mov rax, rsi")
  (emit "    or rax, ~a" pairtag)
  (emit "    add rsi, ~a" (* 2 wordsize)))

(define-primitive (car si env pair)
  ;; assert destination is really a pair ?
  (emit-expr si env pair)
  ;; Subtracting the tag from the heap pointer gets us back the real address.
  (emit "    mov rax, [rax - ~s]         # (car ~a) " pairtag pair))

(define-primitive (cdr si env pair)
  ;; assert destination is really a pair ?
  (emit-expr si env pair)
  ;; Offset for cdr is (address - pairtag + 8) = 5
  (emit "    mov rax, [rax + ~s]         # (cdr ~a) " (- 8 pairtag) pair))
