(load "tests-driver.scm")
(load "tests-1.9-req.scm")
(load "tests-1.8-req.scm")
(load "tests-1.7-req.scm")
(load "tests-1.6-opt.scm")
(load "tests-1.6-req.scm")
(load "tests-1.5-req.scm")
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
(define objshift       3)
(define objmask     #x07)
(define pairtag     #x01)
(define pairsize      16)
(define paircar        0)
(define paircdr        8)
(define vectortag   #x05)
(define stringtag   #x06)
(define wordsize       8) ; bytes
(define wordshift      3)
(define bytes          4)

(define registers
  '((rax scratch)
    (rbx preserve)
    (rcx scratch)
    (rdx scratch)
    (rsi preserve)
    (rdi preserve)
    (rbp preserve)
    (rsp preserve)))
(define (reg-name reg) (car reg))
(define (reg-preserve? reg) (eq? 'preserve (cadr reg)))

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
  (emit "  mov $~s, %rax" (immediate-rep x)))

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

(define (emit-primcall si env expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "  add $~s, %rax" (immediate-rep 1)))

(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "  sub $~s, %rax" (immediate-rep 1)))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "  shl $~s, %rax" (- charshift fxshift))
  (emit "  or $~s, %rax" chartag))

(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "  shr $~s, %rax" (- charshift fxshift)))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "  and $~s, %al" fxmask)
  (emit "  cmp $~s, %al" fxtag)
  (emit-cmp-bool))

(define (emit-cmp-bool . args)
  (emit "  ~s %al" (if (null? args) 'sete (car args)))
  (emit "  movzbq %al, %rax")
  (emit "  sal $~s, %al" bool-bit)
  (emit "  or $~s, %al" bool-f))

(define-primitive (fxzero? si env arg)
  (emit-expr si env arg)
  (emit "  cmp $~s, %rax" fxtag)
  (emit-cmp-bool))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "  cmp $~s, %al" list-nil)
  (emit-cmp-bool))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "  and $~s, %al" boolmask)
  (emit "  cmp $~s, %al" bool-f)
  (emit-cmp-bool))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "  and $~s, %al" charmask)
  (emit "  cmp $~s, %al" chartag)
  (emit-cmp-bool))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit "  cmp $~s, %al" bool-f)
  (emit-cmp-bool))

(define-primitive (fxlognot si env arg)
  (emit-expr si env arg)
  (emit "  shr $~s, %rax" fxshift)
  (emit "  not %rax")
  (emit "  shl $~s, %rax" fxshift))

(define-primitive (fx+ si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  add ~s(%rsp), %rax" si))

(define (emit-binop si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env arg2))

(define (emit-stack-save si)
  (emit "  mov %rax, ~s(%rsp)" si))

(define (emit-stack-load si)
  (emit "  mov ~s(%rsp), %rax" si))

(define (next-stack-index si)
  (- si wordsize))

(define-primitive (fx- si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  sub %rax, ~s(%rsp)" si)
  (emit-stack-load si))

(define-primitive (fx* si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  shr $~s, %rax" fxshift)
  (emit "  mulq ~s(%rsp)" si))

(define-primitive (fxlogor si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  or ~s(%rsp), %rax" si))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  and ~s(%rsp), %rax" si))

(define-primitive (fx= si env arg1 arg2)
  (emit-cmp-binop 'sete si env arg1 arg2))

(define (emit-cmp-binop setx si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  cmp %rax, ~s(%rsp)" si)
  (emit-cmp-bool setx))

(define-primitive (fx< si env arg1 arg2)
  (emit-cmp-binop 'setl si env arg1 arg2))

(define-primitive (fx<= si env arg1 arg2)
  (emit-cmp-binop 'setle si env arg1 arg2))

(define-primitive (fx> si env arg1 arg2)
  (emit-cmp-binop 'setg si env arg1 arg2))

(define-primitive (fx>= si env arg1 arg2)
  (emit-cmp-binop 'setge si env arg1 arg2))

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        L))))

(define (unique-labels lvars)
  (map (lambda (lvar) (format "L_proc_~s" lvar)) lvars))

(define (if? expr)
  (and (list? expr) (eq? (car expr) 'if) (= 3 (length (cdr expr)))))
(define if-test cadr)
(define if-conseq caddr)
(define if-altern cadddr)

(define (emit-if si env tail expr)
  (let ([alt-label (unique-label)]
        [end-label (unique-label)])
    (emit-expr si env (if-test expr))
    (emit "  cmp $~s, %al" bool-f)
    (emit "  je ~a" alt-label)
    (emit-any-expr si env tail (if-conseq expr))
    (if (not tail) (emit "  jmp ~a" end-label))
    (emit-label alt-label)
    (emit-any-expr si env tail (if-altern expr))
    (emit-label end-label)))

(define variable? symbol?)
(define (tagged-list tag expr)
  (and (list? expr) (not (null? expr)) (eq? (car expr) tag)))

(define (make-begin seq) (cons 'begin seq))
(define (begin? expr) (and (tagged-list 'begin expr) (not (null? (begin-seq expr)))))
(define begin-seq cdr)
(define (emit-begin si env tail expr)
  (emit-seq si env tail (begin-seq expr)))
(define (emit-seq si env tail seq)
  (cond
   [(null? seq) (error 'emit-seq "empty seq")]
   [(null? (rest seq)) (emit-any-expr si env tail (first seq))]
   [else
    (emit-expr si env (first seq))
    (emit-seq si env tail (rest seq))]))

(define (let-form? let-kind expr)
  (and (tagged-list let-kind expr)
       (not (null? (cddr expr)))))
(define (let? expr) (let-form? 'let expr))
(define (let*? expr) (let-form? 'let* expr))
(define (letrec? expr) (let-form? 'letrec expr))
(define let-bindings cadr)
(define letrec-bindings let-bindings)
(define (let-body expr)
  (if (null? (cdddr expr))
    (caddr expr)
    (make-begin (cddr expr))))
(define letrec-body let-body)
(define empty? null?)
(define first car)
(define rest cdr)
(define rhs cadr)
(define (lhs binding)
  (check-variable (car binding)))
(define (check-variable var)
  (if (variable? var)
      var
      (error 'lhs (format "~s is not a variable" var))))
(define (make-initial-env lvars labels)
  (map list lvars labels))
(define (extend-env var si new-env)
  (cons (list var si) new-env))
(define (lookup var env)
  (cond
   [(assv var env) => cadr]
   [else #f]))

(define (emit-let si env tail expr)
  (define (process-let bindings si new-env)
    (cond
     [(empty? bindings)
      (emit-any-expr si new-env tail (let-body expr))]
     [else
      (let ([b (first bindings)])
        (emit-expr si (if (let*? expr) new-env env) (rhs b))
        (emit-stack-save si)
        (process-let (rest bindings)
           (next-stack-index si)
           (extend-env (lhs b) si new-env)))]))
  (process-let (let-bindings expr) si env))

(define (emit-variable-ref env var)
  (cond
   [(lookup var env) => emit-stack-load]
   (else (error 'emit-variable-ref (format "undefined variable ~s" var)))))

(define (emit-ret-if tail)
  (if tail (emit "  ret")))

(define (emit-expr si env expr)
  (emit-any-expr si env #f expr))

(define (emit-tail-expr si env expr)
  (emit-any-expr si env #t expr))

(define (emit-any-expr si env tail expr)
  (cond
   [(immediate? expr) (emit-immediate expr) (emit-ret-if tail)]
   [(variable? expr) (emit-variable-ref env expr) (emit-ret-if tail)]
   [(if? expr) (emit-if si env tail expr)]
   [(or (let? expr) (let*? expr)) (emit-let si env tail expr)]
   [(begin? expr) (emit-begin si env tail expr)]
   [(primcall? expr) (emit-primcall si env expr) (emit-ret-if tail)]
   [(app? expr env) (emit-app si env tail expr)]
   [else (error 'emit-expr (format "~s is not an expression" expr))]))

(define (emit-letrec expr)
  (let* ([bindings (letrec-bindings expr)]
         [lvars (map lhs bindings)]
         [lambdas (map rhs bindings)]
         [labels (unique-labels lvars)]
         [env (make-initial-env lvars labels)])
    (for-each (emit-lambda env) lambdas labels)
    (emit-scheme-entry (letrec-body expr) env)))

(define lambda-formals cadr)
(define lambda-body caddr)

(define (emit-lambda env)
  (lambda (expr label)
    (emit-function-header label)
    (let ([fmls (lambda-formals expr)]
          [body (lambda-body expr)])
      (let f ([fmls fmls] [si (- wordsize)] [env env])
        (cond
         [(empty? fmls)
          (emit-tail-expr si env body)]
         [else
          (f (rest fmls)
             (- si wordsize)
             (extend-env (first fmls) si env))])))))

(define (app? expr env)
  (and (list? expr) (not (null? expr)) (lookup (call-target expr) env)))
(define call-target car)
(define call-args cdr)
(define (emit-app si env tail expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env (first args))
      (emit-stack-save si)
      (emit-arguments (- si wordsize) (rest args))))
  (define (move-arguments si delta args)
    (unless (or (= delta 0) (empty? args))
      (emit-stack-load si)
      (emit-stack-save (+ si delta))
      (move-arguments (- si wordsize) delta (rest args))))
  (cond
   [(not tail)
    (emit-arguments (- si wordsize) (call-args expr))
    (emit-adjust-base (+ si wordsize))
    (emit-call (lookup (call-target expr) env))
    (emit-adjust-base (- (+ si wordsize)))]
   [else ; tail
    (emit-arguments si (call-args expr))
    (move-arguments si (- (+ si wordsize)) (call-args expr))
    (emit-jmp (lookup (call-target expr) env))]))

(define heap-cell-size (ash 1 objshift))
(define (emit-heap-alloc size)
  (let ([alloc-size (* (add1 (div (sub1 size) heap-cell-size)) heap-cell-size)])
    (emit "  mov %rbp, %rax")
    (emit "  add $~s, %rbp" (* alloc-size bytes))))
(define (emit-heap-alloc-dynamic)
  (emit "  sub $1, %rax")
  (emit "  shr $~s, %rax" objshift)
  (emit "  add $1, %rax")
  (emit "  shl $~s, %rax" (+ objshift 2))
  (emit "  mov %rbp, %rdx")
  (emit "  add %rax, %rbp")
  (emit "  mov %rdx, %rax"))
(define (emit-stack-to-heap si offset)
  (emit "  mov ~s(%rsp), %rdx" si)
  (emit "  mov %rdx, ~s(%rax)" offset))
(define (emit-heap-load offset)
  (emit "  mov ~s(%rax), %rax" offset))
(define (emit-object? tag si env arg)
  (emit-expr si env arg)
  (emit "  and $~s, %al" objmask)
  (emit "  cmp $~s, %al" tag)
  (emit-cmp-bool))

(define-primitive (make-string si env length)
  (emit-expr-save si env length)
  (emit "  shr $~s, %rax" fxshift)
  (emit "  add $~s, %rax" wordsize)
  (emit-heap-alloc-dynamic)
  (emit-stack-to-heap si 0)
  (emit "  or $~s, %rax" stringtag))
(define-primitive (string? si env arg)
  (emit-object? stringtag si env arg))
(define-primitive (string-set! si env string index value)
  (emit-expr si env index)
  (emit "  shr $~s, %rax" fxshift)
  (emit "  add $~s, %rax" wordsize)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env value)
  (emit "  shr $~s, %rax" charshift)
  (emit-stack-save (next-stack-index si))
  (emit-expr si env string)
  (emit "  add ~s(%rsp), %rax" si)
  (emit "  mov ~s(%rsp), %rdx" (next-stack-index si))
  (emit "  movb %dl, ~s(%rax)" (- stringtag)))
(define-primitive (string-ref si env string index)
  (emit-expr si env index)
  (emit "  shr $~s, %rax" fxshift)
  (emit "  add $~s, %rax" wordsize)
  (emit-stack-save si)
  (emit-expr si env string)
  (emit "  add ~s(%rsp), %rax" si)
  (emit "  movzbq ~s(%rax), %rax" (- stringtag))
  (emit "  shl $~s, %rax" charshift)
  (emit "  or $~s, %rax" chartag))
(define-primitive (string-length si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- stringtag)))
(define-primitive (char= si env arg1 arg2)
  (emit-cmp-binop 'sete si env arg1 arg2))

(define-primitive (make-vector si env length)
  (emit-expr si env length)
  (emit-stack-save si)
  (emit "  shr $~s, %rax" fxshift)
  (emit "  add $1, %rax")
  (emit "  shl $~s, %rax" wordshift)
  (emit-heap-alloc-dynamic)
  (emit-stack-to-heap si 0)
  (emit "  or $~s, %rax" vectortag))
(define-primitive (vector? si env arg)
  (emit-object? vectortag si env arg))
(define-primitive (vector-length si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- vectortag)))
(define-primitive (vector-set! si env vector index value)
  (emit-expr si env index)
  (emit "  shl $~s, %rax" (- objshift fxshift))
  (emit "  add $~s, %rax" wordsize)
  (emit-stack-save si)
  (emit-expr-save (next-stack-index si) env value)
  (emit-expr si env vector)
  (emit "  add ~s(%rsp), %rax" si)
  (emit-stack-to-heap (next-stack-index si) (- vectortag)))
(define-primitive (vector-ref si env vector index)
  (emit-expr si env index)
  (emit "  shl $~s, %rax" (- objshift fxshift))
  (emit "  add $~s, %rax" wordsize)
  (emit-stack-save si)
  (emit-expr si env vector)
  (emit "  add ~s(%rsp), %rax" si)
  (emit-heap-load (- vectortag)))

(define (emit-expr-save si env arg)
  (emit-expr si env arg)
  (emit-stack-save si))

(define-primitive (cons si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit-stack-save (next-stack-index si))
  (emit-heap-alloc pairsize)
  (emit "  or $~s, %rax" pairtag)
  (emit-stack-to-heap si (- paircar pairtag))
  (emit-stack-to-heap (next-stack-index si) (- paircdr pairtag)))
(define-primitive (pair? si env arg)
  (emit-object? pairtag si env arg))
(define-primitive (car si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- paircar pairtag)))
(define-primitive (cdr si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- paircdr pairtag)))
(define-primitive (set-car! si env cell val)
  (emit-binop si env val cell)
  (emit-stack-to-heap si (- paircar pairtag)))
(define-primitive (set-cdr! si env cell val)
  (emit-binop si env val cell)
  (emit-stack-to-heap si (- paircdr pairtag)))
(define-primitive (eq? si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  cmp ~s(%rsp), %rax" si)
  (emit-cmp-bool))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-function-header f)
  (emit "  .text")
  (emit "  .globl ~a" f)
  (emit "  .type ~a, @function" f)
  (emit-label f))

(define (emit-scheme-entry expr env)
  (emit-function-header "L_scheme_entry")
  (emit-tail-expr (- wordsize) env expr))

(define (emit-adjust-base si)
  (unless (= si 0) (emit "  add $~s, %rsp" si)))

(define (emit-call label)
  (emit "  call ~a" label))

(define (emit-jmp label)
  (emit "  jmp ~a" label))

(define (preserve-registers cmd)
  (let loop ([regs registers] [count 0])
    (unless (null? regs)
      (let ([reg (first regs)])
        (if (reg-preserve? reg)
          (cmd (reg-name reg) (* count wordsize)))
        (loop (rest regs) (+ count 1))))))

(define (backup-registers)
  (preserve-registers (lambda (name num)
    (emit "  mov %~a, ~s(%rcx)" name num))))

(define (restore-registers)
  (preserve-registers (lambda (name num)
    (emit "  mov ~s(%rcx), %~a" num name))))
    
(define (emit-program program)
  (emit-function-header "scheme_entry")
  (emit "  mov ~s(%rsp), %rcx" wordsize)
  (backup-registers)
  (emit "  mov ~s(%rsp), %rbp" (* 3 wordsize))
  (emit-call "L_scheme_entry")
  (restore-registers)
  (emit "  ret")
  (cond 
   [(letrec? program) (emit-letrec program)]
   [else (emit-scheme-entry program (make-initial-env '() '()))]))
