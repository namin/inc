(load "tests-driver.scm")
(load "tests-2.6-req.scm")
(load "tests-2.4-req.scm")
(load "tests-2.3-req.scm")
(load "tests-2.2-req.scm")
(load "tests-2.1-req.scm")
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
(define pairsize       8)
(define paircar        0)
(define paircdr        4)
(define vectortag   #x05)
(define stringtag   #x06)
(define closuretag  #x02)
(define wordsize       4) ; bytes
(define wordshift      2)
(define bytes          4)

(define registers
  '((eax scratch)
    (ebx preserve)
    (ecx scratch)
    (edx scratch)
    (esi preserve)
    (edi preserve)
    (ebp preserve)
    (esp preserve)))
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
  (emit "  mov $~s, %eax" (immediate-rep x)))

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
         (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
         (lambda (si env arg* ...) b b* ...)))]))

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
    (or (check-primcall-args prim args)
        (error 'emit-primcall (format "incorrect number of arguments to ~s" prim)))
    (apply (primitive-emitter prim) si env args)))

(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "  add $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "  sub $~s, %eax" (immediate-rep 1)))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "  shl $~s, %eax" (- charshift fxshift))
  (emit "  or $~s, %eax" chartag))

(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "  shr $~s, %eax" (- charshift fxshift)))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "  and $~s, %al" fxmask)
  (emit "  cmp $~s, %al" fxtag)
  (emit-cmp-bool))

(define (emit-cmp-bool . args)
  (emit "  ~s %al" (if (null? args) 'sete (car args)))
  (emit "  movzb %al, %eax")
  (emit "  sal $~s, %al" bool-bit)
  (emit "  or $~s, %al" bool-f))

(define-primitive (fxzero? si env arg)
  (emit-expr si env arg)
  (emit "  cmp $~s, %eax" fxtag)
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
  (emit "  shr $~s, %eax" fxshift)
  (emit "  not %eax")
  (emit "  shl $~s, %eax" fxshift))

(define-primitive (fx+ si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  add ~s(%esp), %eax" si))

(define (emit-binop si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env arg2))

(define (emit-stack-save si)
  (emit "  mov %eax, ~s(%esp)" si))

(define (emit-stack-load si)
  (emit "  mov ~s(%esp), %eax" si))

(define (next-stack-index si)
  (- si wordsize))

(define-primitive (fx- si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  sub %eax, ~s(%esp)" si)
  (emit-stack-load si))

(define-primitive (fx* si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  shr $~s, %eax" fxshift)
  (emit "  mull ~s(%esp)" si))

(define-primitive (fxlogor si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  or ~s(%esp), %eax" si))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  and ~s(%esp), %eax" si))

(define-primitive (fx= si env arg1 arg2)
  (emit-cmp-binop 'sete si env arg1 arg2))

(define (emit-cmp-binop setx si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit "  cmp %eax, ~s(%esp)" si)
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
      (let ([L (string->symbol (format "L_~s" count))])
        (set! count (add1 count))
        L))))

(define (if? expr)
  (and (tagged-list 'if expr)
       (or (= 3 (length (cdr expr)))
           (error 'if? (format "malformed if ~s" expr)))))
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

(define (make-begin lst) 
  (if (null? (cdr lst))
      (car lst)
      (cons 'begin lst)))
(define (begin? expr)
  (and (tagged-list 'begin expr)
       (or (not (null? (begin-seq expr)))
           (error 'begin? (format "empty begin")))))
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

(define (make-set! lhs rhs)
  (list 'set! lhs rhs))
(define (set? expr)
  (tagged-list 'set! expr))
(define set-lhs cadr)
(define set-rhs caddr)
(define make-let list)
(define (let-form? let-kind expr)
  (and (tagged-list let-kind expr)
       (or (not (null? (cddr expr)))
           (error 'let-form? (format "let without body ~s" expr)))))
(define let-kind car)
(define (any-let? expr)
  (and (pair? expr)
       (member (let-kind expr) '(let let* letrec))
       (let-form? (let-kind expr) expr)))
(define (let? expr) (let-form? 'let expr))
(define (let*? expr) (let-form? 'let* expr))
(define (letrec? expr) (or (let-form? 'letrec expr) (let-form? 'letrec* expr)))
(define let-bindings cadr)
(define letrec-bindings let-bindings)
(define labels-bindings let-bindings)
(define make-body make-begin)
(define let-body-seq cddr)
(define (let-body expr)
  (make-body (let-body-seq expr)))
(define letrec-body let-body)
(define labels-body let-body)
(define empty? null?)
(define (bind lhs rhs)
  (check-variable lhs)
  (list lhs rhs))
(define first car)
(define rest cdr)
(define rhs cadr)
(define (lhs binding)
  (check-variable (car binding)))
(define (check-variable var)
  (if (variable? var)
      var
      (error 'lhs (format "~s is not a variable" var))))
(define (make-initial-env bindings)
  bindings)
(define (bulk-extend-env vars vals env)
  (append (map list vars vals) env))
(define (extend-env var si env)
  (cons (list var si) env))
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
        (emit-expr si env (rhs b))
        (emit-stack-save si)
        (process-let (rest bindings)
           (next-stack-index si)
           (extend-env (lhs b) si new-env)))]))
  (process-let (let-bindings expr) si env))

(define (extend-env-with si env lvars k)
  (if (null? lvars)
      (k si env)
      (extend-env-with
       (next-stack-index si)
       (extend-env (first lvars) si env)
       (rest lvars)
       k)))

(define (free-var offset)
  (list 'free (- offset closuretag)))
(define (free-var? fv)
  (tagged-list 'free fv))
(define free-var-offset cadr)

(define (close-env-with offset env lvars k)
  (if (null? lvars)
      (k env)
      (close-env-with
       (+ offset wordsize)
       (extend-env (first lvars) (free-var offset) env)
       (rest lvars)
       k)))

(define (emit-variable-ref si env var)
  (cond
   [(lookup var env) =>
    (lambda (v)
      (cond 
       [(free-var? v)
        (emit "  mov ~s(%edi), %eax" (free-var-offset v))]
       [(number? v)
        (emit-stack-load v)]
       [else (error 'emit-variable-ref (format "looked up unknown value ~s for var ~s" v var))]))]
   [else (error 'emit-variable-ref (format "undefined variable ~s" var))]))

(define (emit-ret-if tail)
  (if tail (emit "  ret")))

(define (emit-expr si env expr)
  (emit-any-expr si env #f expr))

(define (emit-tail-expr si env expr)
  (emit-any-expr si env #t expr))

(define (emit-any-expr si env tail expr)
  (cond
   [(immediate? expr) (emit-immediate expr) (emit-ret-if tail)]
   [(variable? expr) (emit-variable-ref si env expr) (emit-ret-if tail)]
   [(closure? expr) (emit-closure si env expr) (emit-ret-if tail)]
   [(if? expr) (emit-if si env tail expr)]
   [(let? expr) (emit-let si env tail expr)]
   [(begin? expr) (emit-begin si env tail expr)]
   [(quote? expr) (emit-quote si env (quote-expr expr)) (emit-ret-if tail)]
   [(primcall? expr) (emit-primcall si env expr) (emit-ret-if tail)]
   [(app? expr env) (emit-app si env tail expr)]
   [else (error 'emit-expr (format "~s is not an expression" expr))]))

(define unique-name
  (let ([counts '()])
    (lambda (name)
      (cond
       [(assv name counts) =>
        (lambda (p)
          (let* ([count (cdr p)]
                 [new-name (string->symbol (format "~s_~s" name count))])
            (set-cdr! p (add1 count))
            new-name))]
       [else
        (set! counts (cons (cons name 1) counts))
        name]))))

;; TODO: support library functions as described in the paper (3.14)
(define (library-hack expr)
  `(letrec ([length (lambda (lst)
                      (if (null? lst)
                          0
                          (fxadd1 (length (cdr lst)))))]
            [vector (lambda args
                      (let ([v (make-vector (length args))])
                        (letrec ([fill (lambda (index args)
                                         (unless (null? args)
                                                 (vector-set! v index (car args))
                                                 (fill (fxadd1 index) (cdr args))))])
                          (fill 0 args)
                          v)))])
     ,expr))
                                                    
(define (macro-expand expr)
  (define (transform expr bound-vars)
    (cond
     [(set? expr)
      (make-set! (set-lhs expr) (transform (set-rhs expr) bound-vars))]
     [(lambda? expr)
      (make-lambda
       (lambda-formals expr)
       (transform (lambda-body expr)
                  (append (lambda-vars expr) bound-vars)))]
     [(let? expr)
      (make-let
       (let-kind expr)
       (map (lambda (binding)
              (bind (lhs binding) (transform (rhs binding) bound-vars)))
            (let-bindings expr))
       (transform (let-body expr)
                  (append (map lhs (let-bindings expr)) bound-vars)))]
     [(let*? expr)
      (transform
       (if (null? (let-bindings expr))
           (let-body expr)
           (make-let
            'let
            (list (first (let-bindings expr)))
            (make-let
             'let*
             (rest (let-bindings expr))
             (let-body expr))))
       bound-vars)]
     [(letrec? expr)
      (transform
       (make-let
        'let
        (map (lambda (binding) (bind (lhs binding) '#f))
             (letrec-bindings expr))
        (make-body
         (append
          (map (lambda (binding) (make-set! (lhs binding) (rhs binding)))
               (letrec-bindings expr))
          (let-body-seq expr))))
       bound-vars)]
     [(tagged-list 'and expr)
      (cond
       [(null? (cdr expr)) #t]
       [(null? (cddr expr)) (transform (cadr expr) bound-vars)]
       [else
        (transform
         `(if ,(cadr expr)
              (and ,@(cddr expr))
              #f)
         bound-vars)])]
     [(tagged-list 'or expr)
      (cond
       [(null? (cdr expr)) #f]
       [(null? (cddr expr)) (transform (cadr expr) bound-vars)]
       [else
        (transform
         `(let ([one ,(cadr expr)]
                [thunk (lambda () (or ,@(cddr expr)))])
            (if one
                one
                (thunk)))
         bound-vars)])]
     [(tagged-list 'when expr)
      (transform
       `(if ,(cadr expr)
            ,(make-begin (cddr expr))
            #f)
       bound-vars)]
     [(tagged-list 'unless expr)
      (transform
       `(when (not ,(cadr expr)) ,@(cddr expr))
       bound-vars)]
     [(tagged-list 'cond expr)
      (transform
       (let* ([conditions (cdr expr)]
              [first-condition (car conditions)]
              [first-test (car first-condition)]
              [first-body (cdr first-condition)]
              [rest (if (null? (cdr conditions)) #f `(cond ,@(cdr conditions)))])
         (cond
          [(and (eq? first-test 'else) (not (member 'else bound-vars)))
           (make-begin first-body)]
          [(null? first-body)
           `(or ,first-test ,rest)]
          [(and (eq? '=> (car first-body)) (not (member '=> bound-vars)))
           `(let ([one ,first-test])
              (if one (,(cadr first-body) one) ,rest))]
          [else
           `(if ,first-test ,(make-begin first-body) ,rest)]))
       bound-vars)]
     [(list? expr) (map (lambda (e) (transform e bound-vars)) expr)]
     [else expr]))
  (transform expr '()))
      
(define (alpha-conversion expr)
  (define (transform expr env)
    (cond
     [(variable? expr)
      (or (lookup expr env)
          (error 'alpha-conversion (format "undefined variable ~s" expr)))]
     [(lambda? expr)
      (let ([new-env (bulk-extend-env
                      (lambda-vars expr)
                      (map unique-name (lambda-vars expr))
                      env)])
        (make-lambda
         (map-formals (lambda (v) (lookup v new-env)) (lambda-formals expr))
         (transform (lambda-body expr) new-env)))]
     [(let? expr)
      (let* ([lvars (map lhs (let-bindings expr))]
            [new-env (bulk-extend-env
                      lvars
                      (map unique-name lvars)
                      env)])
        (make-let
         'let
         (map (lambda (binding)
                (bind (lookup (lhs binding) new-env)
                      (transform (rhs binding) env)))
              (let-bindings expr))
         (transform (let-body expr) new-env)))]
     [(and (list? expr) (not (null? expr)) (special? (car expr)))
      (cons (car expr) (map (lambda (e) (transform e env)) (cdr expr)))]
     [(list? expr) (map (lambda (e) (transform e env)) expr)]
     [else expr]))
  (transform expr (make-initial-env '())))

(define (assignment-conversion expr)
  (let ([assigned '()])
    (define (set-variable-assigned! v)
      (unless (member v assigned)
              (set! assigned (cons v assigned))))
    (define (variable-assigned v)
      (member v assigned))
    (define (mark expr)
      (when (set? expr) (set-variable-assigned! (set-lhs expr)))
      (when (list? expr) (for-each mark expr)))
    (define (transform expr)
      (cond
       [(set? expr) 
        `(set-car! ,(set-lhs expr) ,(transform (set-rhs expr)))]
       [(lambda? expr)
        (let ([vars (filter variable-assigned (lambda-vars expr))])
          (make-lambda
           (lambda-formals expr)
           (if (null? vars)
               (transform (lambda-body expr))
               (make-let
                'let
                (map (lambda (v) (bind v `(cons ,v #f))) vars)
                (transform (lambda-body expr))))))]
       [(let? expr)
        (make-let
         'let
         (map (lambda (binding)
                (let ([var (lhs binding)]
                      [val (transform (rhs binding))])
                  (bind var
                        (if (variable-assigned var)
                            `(cons ,val #f)
                            val))))
              (let-bindings expr))
         (transform (let-body expr)))]
       [(list? expr) (map transform expr)]
       [(and (variable? expr) (variable-assigned expr))
        `(car ,expr)]
       [else expr]))
    (mark expr)
    (transform expr)))

(define (quote? expr)
  (tagged-list 'quote expr))
(define quote-expr cadr)
(define (emit-quote si env expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(pair? expr)
    (emit-quote si env (car expr))
    (emit-stack-save si)
    (emit-quote (next-stack-index si) env (cdr expr))
    (emit-stack-save (next-stack-index si))
    (emit-cons si)]
   [(vector? expr)
    (emit-expr si env (vector-length expr))
    (emit-stack-save si)
    (emit-make-vector si)
    (emit-stack-save si)
    (let loop ([index 0])
      (unless (= index (vector-length expr))
        (emit-quote (next-stack-index si) env (vector-ref expr index))
        (emit-stack-save (next-stack-index si))
        (emit-stack-load si)
        (emit "  add $~s, %eax" (* wordsize (add1 index)))
        (emit-stack-to-heap (next-stack-index si) (- vectortag))
        (loop (add1 index))))
    (emit-stack-load si)]
   [(string? expr)
    (emit-expr si env (string-length expr))
    (emit-stack-save si)
    (emit-make-string si)
    (emit-stack-save si)
    (let loop ([index 0])
      (unless (= index (string-length expr))
        (emit "  add $~s, %eax" (if (= index 0) wordsize 1))
        (emit "  movb $~s, ~s(%eax)" (char->integer (string-ref expr index)) (- stringtag))
        (loop (add1 index))))
    (emit-stack-load si)]
   [else (error 'emit-quote (format "don't know how to quote ~s" expr))]))

(define (lift-constants expr)
  (let ([constants '()])
    (define (transform expr)
      (cond
       [(and (quote? expr) (assoc expr constants)) => cadr]
       [(quote? expr)
        (set! constants (cons (list expr (unique-name 'c)) constants))
        (cadr (assoc expr constants))]
       [(string? expr) (transform `(quote ,expr))]
       [(list? expr) (map transform expr)]
       [else expr]))
    (let ([texpr (transform expr)])
      (if (null? constants)
          expr
          (make-let
           'let
           (map (lambda (val-cst)
                  (bind (cadr val-cst) (car val-cst)))
                constants)
           texpr)))))
  
(define (closure-conversion expr)
  (let ([labels '()])
    (define (transform expr . label)
      (cond
       [(lambda? expr)
        (let ([label (or (and (not (null? label)) (car label)) (unique-label))]
              [fvs (free-vars expr)])
          (set! labels
                (cons (bind label
                            (make-code (lambda-formals expr)
                                       fvs
                                       (transform (lambda-body expr))))
                    labels))
          (make-closure label fvs))]
       [(any-let? expr)
        (make-let (let-kind expr)
                  (map (lambda (binding)
                         (bind (lhs binding) (transform (rhs binding))))
                       (let-bindings expr))
                  (transform (let-body expr)))]
       [(list? expr)
        (map transform expr)]
       [else
        expr]))
    (let* ([body (transform expr)])
      (make-let 'labels labels body))))

(define (all-conversions expr)
  (closure-conversion (lift-constants (assignment-conversion (alpha-conversion (macro-expand (library-hack expr)))))))

(define (special? symbol)
  (or (member symbol '(if begin let lambda closure set! quote))
      (primitive? symbol)))

(define (flatmap f . lst)
  (apply append (apply map f lst)))

(define (free-vars expr)
  (cond
   [(variable? expr) (list expr)]
   [(lambda? expr) (filter (lambda (v) (not (member v (lambda-vars expr))))
                           (free-vars (lambda-body expr)))]
   [(let? expr)
    (append
     (flatmap free-vars (map rhs (let-bindings expr)))
     (filter (lambda (v) (not (member v (map lhs (let-bindings expr)))))
             (free-vars (let-body expr))))]
   [(list? expr) (flatmap free-vars (if (and (not (null? expr)) (special? (car expr))) (cdr expr) expr))]
   [else '()]))

(define (emit-labels expr)
  (let* ([bindings (labels-bindings expr)]
         [labels (map lhs bindings)]
         [codes (map rhs bindings)]
         [env (make-initial-env '())])
    (for-each (emit-code env) codes labels)
    (emit-scheme-entry (labels-body expr) env)))

(define (lambda? expr) (tagged-list 'lambda expr))
(define lambda-formals cadr)
(define (formals-to-vars formals)
  (cond
   [(list? formals) formals]
   [(pair? formals) (cons (car formals) (formals-to-vars (cdr formals)))]
   [else (list formals)]))
(define (lambda-vars expr) (formals-to-vars (lambda-formals expr)))
(define (map-formals f formals)
  (cond
   [(list? formals) (map f formals)]
   [(pair? formals) (cons (f (car formals)) (map-formals f (cdr formals)))]
   [else (f formals)]))
(define (lambda-body expr) (make-body (cddr expr)))
(define (make-lambda formals body)
  (list 'lambda formals body))

(define (make-closure label fvs)
  (cons 'closure (cons label fvs)))
(define (closure? expr) (tagged-list 'closure expr))
(define closure-label cadr)
(define closure-free-vars cddr)
(define (emit-closure si env expr)
  (let ([label (closure-label expr)]
        [fvs (closure-free-vars expr)])
    (emit-heap-alloc (* (add1 (length fvs)) wordsize))
    (emit "  movl $~s, (%eax)" label)
    (unless (null? fvs)
      (emit "  mov %eax, %edx")
      (let loop ([fvs fvs] [count 1])
        (unless (null? fvs)
          (emit-variable-ref si env (first fvs))
          (emit "  mov %eax, ~s(%edx)" (* count wordsize))
          (loop (rest fvs) (add1 count))))
      (emit "  mov %edx, %eax"))
    (emit "  or $~s, %eax" closuretag)))

(define (make-code formals free body)
  (list 'code formals free body))
(define code-formals cadr)
(define (code-bound-variables expr) (formals-to-vars (code-formals expr)))
(define (code-vararg? expr)
  (not (list? (code-formals expr))))
(define code-free-variables caddr)
(define code-body cadddr)
(define (emit-code env)
  (lambda (expr label)
    (emit-function-header label)
    (let ([bvs (code-bound-variables expr)]
          [fvs (code-free-variables expr)]
          [body (code-body expr)])
      (when (code-vararg? expr)
            (let ([start-label (unique-label)]
                  [fill-label (unique-label)]
                  [loop-label (unique-label)])
              (emit "  mov %eax, %edx")
              (emit-immediate '())
              (emit "  cmp $~s, %edx" (- (length bvs) 1))
              (emit "  jle ~a" fill-label)
              (emit-label loop-label)
              (emit "  cmp $~s, %edx" (length bvs))
              (emit "  jl ~a" start-label)
              (emit "  shl $~s, %edx" wordshift)
              (emit "  sub %edx, %esp")
              (emit-stack-save (next-stack-index 0))
              (emit "  mov %edx, %eax")
              (emit-stack-save (next-stack-index (next-stack-index 0)))
              (emit-cons 0)
              (emit-stack-save 0)
              (emit-stack-load (next-stack-index (next-stack-index 0)))
              (emit "  mov %eax, %edx")
              (emit-stack-load 0)
              (emit "  add %edx, %esp")
              (emit "  shr $~s, %edx" wordshift)
              (emit "  sub $1, %edx")
              (emit-jmp loop-label)
              (emit-label fill-label)
              (emit "  add $1, %edx")
              (emit "  shl $~s, %edx" wordshift)
              (emit "  sub %edx, %esp")
              (emit-stack-save 0)
              (emit "  add %edx, %esp")
              (emit-label start-label)))
      (extend-env-with (- wordsize) env bvs (lambda (si env)
        (close-env-with wordsize env fvs (lambda (env)
          (emit-tail-expr si env body))))))))

(define (app? expr env)
  (and (list? expr) (not (null? expr))))
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
    (emit-arguments (- si (* 2 wordsize)) (call-args expr))
    (emit-expr si env (call-target expr))
    (emit "  mov %edi, ~s(%esp)" si)
    (emit "  mov %eax, %edi")
    (emit-heap-load (- closuretag))
    (emit-adjust-base si)
    (emit "  mov %eax, %edx")
    (emit "  mov $~s, %eax" (length (call-args expr)))
    (emit-call "*%edx")
    (emit-adjust-base (- si))
    (emit "  mov ~s(%esp), %edi" si)]
   [else ; tail
    (emit-arguments si (call-args expr))
    (emit-expr (- si (* wordsize (length (call-args expr)))) env (call-target expr))
    (emit "  mov %eax, %edi")
    (move-arguments si (- (+ si wordsize)) (call-args expr))
    (emit "  mov %edi, %eax")
    (emit-heap-load (- closuretag))
    (emit "  mov %eax, %edx")
    (emit "  mov $~s, %eax" (length (call-args expr)))
    (emit-jmp "*%edx")]))

(define heap-cell-size (ash 1 objshift))
(define (emit-heap-alloc size)
  (let ([alloc-size (* (add1 (div (sub1 size) heap-cell-size)) heap-cell-size)])
    (emit "  mov %ebp, %eax")
    (emit "  add $~s, %ebp" (* alloc-size bytes))))
(define (emit-heap-alloc-dynamic)
  (emit "  add $~s, %eax" (sub1 heap-cell-size))
  (emit "  and $~s, %eax" (- heap-cell-size))
  (emit "  mov %ebp, %edx")
  (emit "  add %eax, %ebp")
  (emit "  mov %edx, %eax"))
(define (emit-stack-to-heap si offset)
  (emit "  mov ~s(%esp), %edx" si)
  (emit "  mov %edx, ~s(%eax)" offset))
(define (emit-heap-load offset)
  (emit "  mov ~s(%eax), %eax" offset))
(define (emit-object? tag si env arg)
  (emit-expr si env arg)
  (emit "  and $~s, %al" objmask)
  (emit "  cmp $~s, %al" tag)
  (emit-cmp-bool))

(define-primitive (make-string si env length)
  (emit-expr-save si env length)
  (emit-make-string si))
(define (emit-make-string si)
  (emit "  shr $~s, %eax" fxshift)
  (emit "  add $~s, %eax" wordsize)
  (emit-heap-alloc-dynamic)
  (emit-stack-to-heap si 0)
  (emit "  or $~s, %eax" stringtag))
(define-primitive (string? si env arg)
  (emit-object? stringtag si env arg))
(define-primitive (string-set! si env string index value)
  (emit-expr si env index)
  (emit "  shr $~s, %eax" fxshift)
  (emit "  add $~s, %eax" wordsize)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env value)
  (emit "  shr $~s, %eax" charshift)
  (emit-stack-save (next-stack-index si))
  (emit-expr si env string)
  (emit "  add ~s(%esp), %eax" si)
  (emit "  mov ~s(%esp), %edx" (next-stack-index si))
  (emit "  movb %dl, ~s(%eax)" (- stringtag)))
(define-primitive (string-ref si env string index)
  (emit-expr si env index)
  (emit "  shr $~s, %eax" fxshift)
  (emit "  add $~s, %eax" wordsize)
  (emit-stack-save si)
  (emit-expr si env string)
  (emit "  add ~s(%esp), %eax" si)
  (emit "  movzb ~s(%eax), %eax" (- stringtag))
  (emit "  shl $~s, %eax" charshift)
  (emit "  or $~s, %eax" chartag))
(define-primitive (string-length si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- stringtag)))
(define-primitive (char= si env arg1 arg2)
  (emit-cmp-binop 'sete si env arg1 arg2))

(define-primitive (make-vector si env length)
  (emit-expr-save si env length)
  (emit-make-vector si))
(define (emit-make-vector si)
  (emit "  add $~s, %eax" wordsize)
  (emit-heap-alloc-dynamic)
  (emit-stack-to-heap si 0)
  (emit "  or $~s, %eax" vectortag))
(define-primitive (vector? si env arg)
  (emit-object? vectortag si env arg))
(define-primitive (vector-length si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- vectortag)))
(define-primitive (vector-set! si env vector index value)
  (emit-expr si env index)
  (emit "  add $~s, %eax" wordsize)
  (emit-stack-save si)
  (emit-expr-save (next-stack-index si) env value)
  (emit-expr si env vector)
  (emit "  add ~s(%esp), %eax" si)
  (emit-stack-to-heap (next-stack-index si) (- vectortag)))
(define-primitive (vector-ref si env vector index)
  (emit-expr si env index)
  (emit "  add $~s, %eax" wordsize)
  (emit-stack-save si)
  (emit-expr si env vector)
  (emit "  add ~s(%esp), %eax" si)
  (emit-heap-load (- vectortag)))

(define-primitive (procedure? si env arg)
  (emit-object? closuretag si env arg))

(define (emit-expr-save si env arg)
  (emit-expr si env arg)
  (emit-stack-save si))

(define-primitive (cons si env arg1 arg2)
  (emit-binop si env arg1 arg2)
  (emit-stack-save (next-stack-index si))
  (emit-cons si))
(define (emit-cons si)
  (emit-heap-alloc pairsize)
  (emit "  or $~s, %eax" pairtag)
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
  (emit "  cmp ~s(%esp), %eax" si)
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
  (cond
   [(> si 0) (emit "  add $~s, %esp" si)]
   [(< si 0) (emit "  sub $~s, %esp" (- si))]))

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
    (emit "  mov %~a, ~s(%ecx)" name num))))

(define (restore-registers)
  (preserve-registers (lambda (name num)
    (emit "  mov ~s(%ecx), %~a" num name))))
    
(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit "  mov 4(%esp), %ecx")
  (backup-registers)
  (emit "  mov 12(%esp), %ebp")
  (emit "  mov 8(%esp), %esp")
  (emit-call "L_scheme_entry")
  (restore-registers)
  (emit "  ret")
  (emit-labels (all-conversions expr)))
