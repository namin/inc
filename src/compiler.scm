(load "tests-driver.scm")
(load "tests-5.3-req.scm")
(load "tests-5.2-req.scm")
(load "tests-4.2-req.scm")
(load "tests-4.1-req.scm")
(load "tests-3.4-req.scm")
(load "tests-3.3-req.scm")
(load "tests-3.2-req.scm")
(load "tests-3.1-req.scm")
(load "tests-2.9-req.scm")
(load "tests-2.8-req.scm")
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
(define eof-obj     #x7F)
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
(define symboltag   #x03)
(define wordsize       4) ; bytes
(define wordshift      2)
(define global-offset  4)
(define edi-offset     8)
(define closure-end #x07)
(define return-addr #x17)

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

(define (make-begin lst) 
  (cond
   [(null? (cdr lst)) (car lst)]
   [else (cons 'begin lst)]))
(define (begin? expr)
  (and (tagged-list 'begin expr)
       (or (not (null? (begin-seq expr)))
           (error 'begin? (format "empty begin")))))
(define begin-seq cdr)
(define (lambda? expr) (tagged-list 'lambda expr))
(define lambda-formals cadr)
(define (formals-to-vars formals)
  (cond
   [(list? formals) (map (lambda (x) (if (list? x) (car x) x)) formals)]
   [(pair? formals) (cons (car formals) (formals-to-vars (cdr formals)))]
   [else (list formals)]))
(define (lambda-vars expr)
  (formals-to-vars (lambda-formals expr)))
(define (map-formals f formals)
  (cond
   [(list? formals) (map (lambda (x) (if (pair? x) (cons (f (car x)) (cdr x)) (f x))) formals)]
   [(pair? formals) (cons (f (car formals)) (map-formals f (cdr formals)))]
   [else (f formals)]))
(define (lambda-body expr) (make-body (cddr expr)))
(define (make-lambda formals body)
  (list 'lambda formals body))

(define lib-primitives '())
(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
         (length '(arg* ...)))
       (putprop 'prim-name '*emitter*
         (lambda (si env arg* ...) b b* ...)))]
    [(_ (prim-name si env arg* ... . vararg) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
         (length '(arg* ...)))
       (putprop 'prim-name '*vararg* #t)
       (putprop 'prim-name '*emitter*
         (lambda (si env arg* ... . vararg) b b* ...)))]))
(define-syntax define-lib-primitive
  (syntax-rules ()
    [(_ (prim-name arg* ...) b b* ...)
     (begin
       (set! lib-primitives (cons 'prim-name lib-primitives))
       (putprop 'prim-name '*is-lib-prim* #t)
       (putprop 'prim-name '*arg-count*
         (length '(arg* ...)))
       (putprop 'prim-name '*lib-code*
         (make-lambda '(arg* ...) (make-begin '(b b* ...)))))]
    [(_ (prim-name . varargs) b b* ...)
     (begin
       (set! lib-primitives (cons 'prim-name lib-primitives))
       (putprop 'prim-name '*is-lib-prim* #t)
       (putprop 'prim-name '*arg-count* 0)
       (putprop 'prim-name '*vararg* #t)
       (putprop 'prim-name '*lib-code*
         (make-lambda 'varargs (make-begin '(b b* ...)))))]
    [(_ prim-name b)
     (begin
       (set! lib-primitives (cons 'prim-name lib-primitives))
       (putprop 'prim-name '*is-lib-prim* #t)
       (putprop 'prim-name '*lib-code* 'b))]))
(load "lib.scm")

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (lib-primitive? x)
  (and (symbol? x) (getprop x '*is-lib-prim*)))

(define (lib-primitive-code x)
  (or (getprop x '*lib-code*) (error 'lib-primitive-code (format "primitive ~s has no lib code" x))))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'primitive-emitter (format "primitive ~s has no emitter" x))))

(define aexpr-primitives '(constant-ref primitive-ref))

(define (aexpr-primcall? expr)
  (and (pair? expr) (primitive? (car expr)) (member (car expr) aexpr-primitives)))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  ((if (getprop prim '*vararg*) <= =) (getprop prim '*arg-count*) (length args)))

(define (emit-any-primcall si env prim args)
  (or (check-primcall-args prim args)
      (error 'emit-primcall (format "incorrect number of arguments to ~s" prim)))
  (apply (primitive-emitter prim) si env args))

(define (emit-aexpr-primcall si env expr)
  (let ([prim (car expr)]
	[args (cdr expr)])
    (emit-any-primcall si env prim args)))

(define (emit-primcall si env expr)
  (let ([prim (car expr)]
	[cont (cadr expr)]
	[args (cddr expr)])
    (emit-any-primcall si env prim args)
    (emit-stack-save si)
    (emit-expr (next-stack-index si) env cont)
    (emit "  mov %eax, %edi")
    (emit-stack-load si)
    (emit-stack-save (- wordsize))
    (emit "  mov %edi, %eax")
    (emit-heap-load (- closuretag))
    (emit "  mov %eax, %edx")
    (emit "  mov $1, %eax")
    (emit-jmp "*%edx")))

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

(define-primitive (eof-object? si env arg)
  (emit-expr si env arg)
  (emit "  cmp $~s, %al" eof-obj)
  (emit-cmp-bool))

(define-primitive (eof-object si env)
  (emit "  mov $~s, %eax" eof-obj))

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

(define (emit-div si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "  shr $~s, %eax" fxshift)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env arg1)
  (emit "  mov $0, %edx")
  (emit "  shr $~s, %eax" fxshift)
  (emit "  divl ~s(%esp)" si))

(define-primitive ($fxquotient si env arg1 arg2)
  (emit-div si env arg1 arg2)
  (emit "  shl $~s, %eax" fxshift))

(define-primitive ($fxremainder si env arg1 arg2)
  (emit-div si env arg1 arg2)
  (emit "  mov %edx, %eax")
  (emit "  shl $~s, %eax" fxshift))
  
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
   [(immediate? expr)      (emit-immediate expr)             (emit-ret-if tail)]
   [(variable? expr)       (emit-variable-ref si env expr)   (emit-ret-if tail)]
   [(closure? expr)        (emit-closure si env expr)        (emit-ret-if tail)]
   [(if? expr)             (emit-if si env tail expr)        (assert      tail)]
   [(let? expr)            (emit-let si env tail expr)       (assert      tail)]
   [(aexpr-primcall? expr) (emit-aexpr-primcall si env expr) (emit-ret-if tail)]
   [(primcall? expr)       (emit-primcall si env expr)       (assert      tail)]
   [(app? expr)            (emit-app si env tail expr)       (assert      tail)]
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
       [(lib-primitive? name)
        (set! counts (cons (cons name 1) counts))
	(unique-name name)]
       [else
        (set! counts (cons (cons name 1) counts))
        name]))))

(define (define? expr)
  (tagged-list 'define expr))
;; TODO: support syntatic (define (f ...) ...) form.
(define (define-lhs expr)
  (cadr expr))
(define (define-rhs expr)
  (make-body (cddr expr)))
(define (macro-expand expr)
  (define (transform expr bound-vars)
    (cond
     [(and (begin? expr) (not (null? (filter define? (begin-seq expr)))))
      (let loop ([prev '()] [defs '()] [todo (begin-seq expr)])
	(cond
	 [(define? (car todo))
	  (loop prev
		(append defs (list (bind (define-lhs (car todo)) (define-rhs (car todo)))))
		(cdr todo))]
	 [(null? defs)
	  (loop (append prev (list (car todo)))
		defs
		(cdr todo))]
	 [else
	  (let ([last (make-let
		       'letrec
		       defs
		       (make-body todo))])
	    (transform
	     (if (null? prev)
		 last
		 (combine-exprs (make-begin prev) last))
	     bound-vars))]))]
     [(set? expr)
      (make-set! (set-lhs expr) (transform (set-rhs expr) bound-vars))]
     [(lambda? expr)
      (let* ([formals (lambda-formals expr)]
	     [optional-args (filter list? (if (list? formals) formals '()))])
	(if (null? optional-args)
	    (make-lambda
	     formals
	     (transform (lambda-body expr)
			(append (lambda-vars expr) bound-vars)))
	    (let ([new-formals (map (lambda (x)
				      (if (list? x)
					  (list (car x))
					  x))
				    formals)]
		  [bindings (map (lambda (var-val)
				   (let ([var (car var-val)]
					 [val (cadr var-val)])
				     (bind var
					   `(if ,var ,var ,val))))
				 optional-args)])
	      (make-lambda
	       new-formals
	       (transform
		(make-let
		 'let*
		 bindings
		 (lambda-body expr))
		(append (lambda-vars expr) bound-vars))))))]
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
          (and (lib-primitive? expr) expr)
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
     [(quote? expr) expr]
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

(define (translate-quote expr)
  (cond
   [(immediate? expr) expr]
   [(symbol? expr) (list 'string->symbol (translate-quote (symbol->string expr)))]
   [(pair? expr)
    (list 'cons (translate-quote (car expr)) (translate-quote (cdr expr)))]
   [(vector? expr)
    (cons 'vector (map translate-quote (vector->list expr)))]
   [(string? expr)
    (cons 'string (map translate-quote (string->list expr)))]
   [else (error 'translate-quote (format "don't know how to quote ~s" expr))]))

(define (lift-constants expr)
  (let ([constants '()])
    (define (transform expr)
      (cond
       [(and (quote? expr) (immediate? (quote-expr expr))) (quote-expr expr)]
       [(and (quote? expr) (assoc expr constants)) => cadr]
       [(quote? expr)
        (set! constants (cons (list expr (list 'constant-ref (unique-name 'c))) constants))
        (cadr (assoc expr constants))]
       [(string? expr) (transform `(quote ,expr))]
       [(foreign-call? expr) (make-foreign-call (foreign-call-name expr) (map transform (foreign-call-args expr)))]
       [(list? expr) (map transform expr)]
       [else expr]))
    (let ([texpr (transform expr)])
      (make-let
       'labels
       (map (lambda (val-cst)
              (bind (cadadr val-cst) '(datum)))
            constants)
       (if (null? constants)
           texpr
           (combine-exprs
            (make-begin
             (map (lambda (val-cst)
                    (list 'constant-init
                          (cadadr val-cst)
                          (all-expr-conversions (translate-quote (quote-expr (car val-cst))))))
                  constants))
            texpr))))))
(define (combine-exprs a b)
  (cond
   [(and (begin? a) (begin? b)) (make-begin (append (begin-seq a) (begin-seq b)))]
   [(begin? a) (make-begin (append (begin-seq a) (list b)))]
   [(begin? b) (make-begin (cons a (begin-seq b)))]
   [else (make-begin (list a b))]))
(define-primitive (constant-ref si env constant)
  (emit "  mov ~s, %eax" constant)
  (emit-global-load))
(define-primitive (constant-init si env constant value)
  (emit ".local ~s" constant)
  (emit ".comm ~s,4,4" constant)
  (emit-expr si env value)
  (emit-global-save)
  (emit "  mov %eax, ~s" constant))

(define (annotate-lib-primitives expr)
  (define (transform expr)
    (cond
     [(and (variable? expr) (lib-primitive? expr)) `(primitive-ref ,expr)]
     [(list? expr) (map transform expr)]
     [else expr]))
  (transform expr))
(define-primitive (primitive-ref si env prim-name)
  (let ([label (primitive-label prim-name)]
        [done-label (unique-label)])
    (emit "  mov ~s, %eax" label)
    (emit "  testl %eax, %eax")
    (emit "  jne ~s" done-label)
    (emit "  movl $~s, ~s(%esp)" return-addr si)
    (emit-adjust-base (next-stack-index si))
    (emit-call (primitive-alloc prim-name))
    (emit-adjust-base (- (next-stack-index si)))
    (emit-global-save)
    (emit "  mov %eax, ~s" label)
    (emit-label done-label)
    (emit-global-load)))

(define (emit-global-save)
  (emit "  mov ~s(%ebp), %edx" global-offset)
  (emit "  mov %eax, (%edx)")
  (emit "  mov %edx, %eax")
  (emit "  add $~s, %edx" wordsize)
  (emit "  mov %edx, ~s(%ebp)" global-offset))

(define (emit-global-load)
  (emit "  mov (%eax), %eax"))

(define (primitive-label name)
  (let ([lst (map (lambda (c)
		    (case c
		      [(#\-) #\_]
		      [(#\!) #\b]
		      [(#\=) #\e]
		      [(#\>) #\g]
		      [(#\?) #\p]
		      [else c]))
		  (string->list (symbol->string name)))])
  (string->symbol (format "P_~a" (list->string lst)))))
(define (primitive-alloc name)
  (string->symbol (format "~a_alloc" (primitive-label name))))

;; Not used, as let can avoid useless closure creation.
(define (macro-expand-let expr)
  (define (transform expr)
    (cond
     [(let? expr)
      (let ([vars (map lhs (let-bindings expr))]
	    [vals (map (lambda (binding) (transform (rhs binding)))
		       (let-bindings expr))])
	(cons (make-lambda
	       vars
	       (transform (let-body expr)))
	      vals))]
     [(list? expr) (map (lambda (e) (transform e)) expr)]
     [else expr]))
  (transform expr))

(define (cps-conversion expr)
  (make-let
   'labels
   (let-bindings expr)
   (cps-top (let-body expr))))
(load "cps.scm")

(define (closure-conversion expr)
  (let ([labels '()]
        [constants (map lhs (labels-bindings expr))])
    (define (transform expr . label)
      (cond
       [(lambda? expr)
        (let ([label (or (and (not (null? label)) (car label)) (unique-label))]
              [fvs (filter (lambda (v) (not (member v constants))) (free-vars expr))]
	      [body (transform (lambda-body expr))])
          (set! labels
                (cons (bind label
                            (make-code (lambda-formals expr)
                                       fvs
                                       body))
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
    (let* ([body (transform (labels-body expr))])
      (make-let 'labels labels body))))

(define (all-expr-conversions expr)
  (annotate-lib-primitives (assignment-conversion (alpha-conversion (macro-expand expr)))))
(define (all-conversions expr)
  (closure-conversion (cps-conversion (lift-constants (all-expr-conversions expr)))))

(define (special? symbol)
  (or (member symbol '(if begin let lambda closure set! quote apply call/cc))
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
   [(tagged-list 'primitive-ref expr) '()]
   [(list? expr) (flatmap free-vars (if (and (not (null? expr)) (special? (car expr))) (cdr expr) expr))]
   [else '()]))

(define (emit-library)
  (define (emit-library-primitive prim-name)
    (let ([labels (all-conversions (lib-primitive-code prim-name))])
      (emit-labels labels (lambda (expr env)
        ((emit-code env #t) (make-code '() '() expr) (primitive-alloc prim-name))))
      (let ([label (primitive-label prim-name)])
        (emit ".global ~s" label)
        (emit ".comm ~s,4,4" label))))
  (for-each emit-library-primitive lib-primitives))

(define (emit-labels expr k)
  (let* ([bindings (labels-bindings expr)]
         [labels (map lhs bindings)]
         [codes (map rhs bindings)]
         [env (make-initial-env '())])
    (for-each (emit-code env #f) codes labels)
    (k (labels-body expr) env)))

(define (make-closure label fvs)
  (cons 'closure (cons label fvs)))
(define (closure? expr) (tagged-list 'closure expr))
(define closure-label cadr)
(define closure-free-vars cddr)
(define (emit-closure si env expr)
  (let ([label (closure-label expr)]
        [fvs (closure-free-vars expr)])
    (emit-heap-alloc-static si (* (+ 2 (length fvs)) wordsize))
    (emit "  movl $~s, (%eax)" label)
    (emit "  mov %eax, %edx")
    (let loop ([fvs fvs] [count 1])
      (cond
       [(null? fvs)
	(emit "  movl $~s, ~s(%edx)" closure-end (* count wordsize))]
       [else
	(emit-variable-ref si env (first fvs))
	(emit "  mov %eax, ~s(%edx)" (* count wordsize))
	(loop (rest fvs) (add1 count))]))
    (emit "  mov %edx, %eax")
    (emit "  or $~s, %eax" closuretag)))

(define (make-code formals free body)
  (list 'code formals free body))
(define code-formals cadr)
(define (code-bound-variables expr) (formals-to-vars (code-formals expr)))
(define (code-vararg? expr)
  (not (list? (code-formals expr))))
(define (code-optarg? expr)
  (and (list? (code-formals expr)) (not (null? (filter list? (code-formals expr))))))
(define (code-opt-start-index expr)
  (let loop ([index 0] [formals (code-formals expr)])
    (if (list? (car formals))
	index
	(loop (add1 index) (cdr formals)))))
(define code-free-variables caddr)
(define code-body cadddr)
(define (emit-code env global?)
  (lambda (expr label)
    ((if global? emit-function-header emit-label) label)
    (let ([bvs (code-bound-variables expr)]
          [fvs (code-free-variables expr)]
          [body (code-body expr)])
      (when (and (not (code-vararg? expr)) (not (code-optarg? expr)))
	    (let ([start-label (unique-label)])
	      (emit "  cmp $~s, %eax" (length bvs))
	      (emit "  je ~a" start-label)
	      (emit-error (- wordsize) env)
	      (emit-label start-label)
	      (emit "  mov $~s, %eax" (length bvs))))
      (when (code-optarg? expr)
	    (let ([start-index (code-opt-start-index expr)]
		  [len (length bvs)]
		  [check2-label (unique-label)]
		  [loop-label (unique-label)]
		  [start-label (unique-label)])
	      (emit "  mov %eax, %edx")
	      (emit "  cmp $~s, %edx" start-index)
	      (emit "  jge ~a" check2-label)
	      (emit-error (- wordsize) env)
	      (emit-label check2-label)
	      (emit "  cmp $~s, %edx" len)
	      (emit "  jle ~a" loop-label)
	      (emit-error (- wordsize) env)
	      (emit-label loop-label)
	      (emit "  cmp $~s, %edx" len)
	      (emit "  je ~a" start-label)
	      (emit "  add $1, %edx")
	      (emit "  mov %edx, %eax")
	      (emit "  shl $~s, %eax" wordshift)
	      (emit "  neg %eax")
	      (emit "  add %esp, %eax")
	      (emit "  movl $~s, (%eax)" bool-f)
	      (emit-jmp loop-label)
	      (emit-label start-label)
	      (emit "  mov %edx, %eax")))
      (when (code-vararg? expr)
            (let ([ok (unique-label)]
		  [start-label (unique-label)]
                  [fill-label (unique-label)]
                  [loop-label (unique-label)])
              (emit "  mov %eax, %edx")
	      (when (> (- (length bvs) 1) 0)
		    (emit "  cmp $~s, %edx" (- (length bvs) 1))
		    (emit "  jge ~a" ok)
		    (emit-error (- wordsize) env)
		    (emit-label ok))
              (emit-immediate '())
              (emit "  cmp $~s, %edx" (- (length bvs) 1))
              (emit "  je ~a" fill-label)
              (emit-label loop-label)
              (emit "  cmp $~s, %edx" (length bvs))
              (emit "  jl ~a" start-label)
              (emit "  shl $~s, %edx" wordshift)
              (emit "  sub %edx, %esp")
              (emit-stack-save (next-stack-index 0))
              (emit "  mov %edx, %eax")
              (emit-stack-save (next-stack-index (next-stack-index 0)))
              (emit-cons 0 (next-stack-index (next-stack-index (next-stack-index 0))))
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
(define (app? expr)
  (and (list? expr) (not (null? expr))))
(define (call-apply? expr)
  (tagged-list 'apply expr))
(define (call-target expr) 
  (if (call-apply? expr)
      (cadr expr)
      (car expr)))
(define (call-args expr)
  (if (call-apply? expr)
      (cddr expr)
      (cdr expr)))
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
  (define (splice-last-argument)
    (let ([si (- (* wordsize (length (call-args expr))))]
	  [loop-label (unique-label)]
	  [pair-label (unique-label)]
	  [done-label (unique-label)])
      (emit-stack-save (next-stack-index si))
      (emit "  mov $~s, %eax" (length (call-args expr)))
      (emit-stack-save (next-stack-index (next-stack-index si)))
      (emit "  mov %eax, %edx")
      (emit-stack-load si)
      (emit-label loop-label)
      (emit "  cmp $~s, %al" list-nil)
      (emit "  jne ~a" pair-label)
      (emit "  mov %edx, %eax")
      (emit "  shl $2, %eax")
      (emit "  neg %eax")
      (emit "  add %esp, %eax")
      (emit "  mov -4(%eax), %edx")
      (emit "  mov -8(%eax), %eax")
      (emit "  sub $1, %eax")
      (emit-jmp done-label)
      (emit-label pair-label)
      (emit "  mov %edx, %eax")
      (emit "  shl $2, %eax")
      (emit "  neg %eax")
      (emit "  add %esp, %eax")
      (emit "  mov -8(%eax), %edx")
      (emit "  add $1, %edx")
      (emit "  mov %edx, -12(%eax)")
      (emit "  mov -4(%eax), %edx")
      (emit "  mov %edx, -8(%eax)")
      (emit "  mov (%eax), %edx")
      (emit "  mov ~s(%edx), %edx" (- paircdr pairtag))
      (emit "  mov %edx, -4(%eax)")
      (emit "  mov (%eax), %edx")
      (emit "  mov ~s(%edx), %edx" (- paircar pairtag))
      (emit "  mov %edx, (%eax)")
      (emit "  mov %eax, %edx")
      (emit "  mov -4(%eax), %eax")
      (emit "  mov -12(%edx), %edx")
      (emit-jmp loop-label)
      (emit-label done-label)))
  (cond
   [(not tail)
    (emit-arguments (- si (* 2 wordsize)) (call-args expr))
    (emit-expr (- si (* wordsize (+ 2 (length (call-args expr))))) env (call-target expr))
    (emit "  mov %edi, ~s(%esp)" si)
    (emit "  mov %eax, %edi")
    (emit-ensure-procedure si env expr)
    (emit-heap-load (- closuretag))
    (emit-adjust-base si)
    (emit "  mov %eax, %edx")
    (if (call-apply? expr)
	(begin
	  (emit "  sub $4, %esp")
	  (splice-last-argument)
	  (emit "  add $4, %esp"))
	(emit "  mov $~s, %eax" (length (call-args expr))))
    (emit-call "*%edx")
    (emit-adjust-base (- si))
    (emit "  mov ~s(%esp), %edi" si)]
   [else ; tail
    (emit-arguments si (call-args expr))
    (emit-expr (- si (* wordsize (length (call-args expr)))) env (call-target expr))
    (emit "  mov %eax, %edi")
    (emit-ensure-procedure si env expr)
    (move-arguments si (- (+ si wordsize)) (call-args expr))
    (emit "  mov %edi, %eax")
    (emit-heap-load (- closuretag))
    (emit "  mov %eax, %edx")
    (if (call-apply? expr)
	(splice-last-argument)
	(emit "  mov $~s, %eax" (length (call-args expr))))
    (emit-jmp "*%edx")]))
(define (emit-ensure-procedure si env expr)
  (unless (equal? (call-target expr) '(primitive-ref error))
    (let ([ok (unique-label)])
      (emit "  and $~s, %al" objmask)
      (emit "  cmp $~s, %al" closuretag)
      (emit "  je ~a" ok)
      (emit-error si env)
      (emit-label ok)
      (emit "  mov %edi, %eax"))))
(define (emit-error si env)
  (emit-tail-expr si env '((primitive-ref error) #f)))
  

(define (foreign-call? expr)
  (tagged-list 'foreign-call expr))
(define (make-foreign-call name args)
  (cons 'foreign-call (cons name args)))
(define foreign-call-name cadr)
(define foreign-call-args cddr)
(define-primitive (foreign-call si env name . args)
  (let ([new-si (let loop ([si (+ si wordsize)]
			   [args (reverse args)])
                  (cond
                   [(null? args) si]
                   [else
                    (emit-expr-save (next-stack-index si) env (car args))
                    (loop (next-stack-index si) (cdr args))]))])
    (emit-adjust-base new-si)
    (emit-call name)
    (emit-adjust-base (- new-si))))
            
(define heap-cell-size (ash 1 objshift))
(define (emit-heap-alloc-static si size)
  (let ([alloc-size (* (add1 (div (sub1 size) heap-cell-size)) heap-cell-size)])
    (emit "  mov $~s, %eax" alloc-size)
    (emit-heap-alloc si)))
(define (emit-heap-alloc-dynamic si)
  (emit "  add $~s, %eax" (sub1 heap-cell-size))
  (emit "  and $~s, %eax" (- heap-cell-size))
  (emit-heap-alloc si))
(define (emit-heap-alloc si)
  (let ([new-si (- si (* 2 wordsize))])
    (emit-adjust-base new-si)
    (emit "  mov %eax, ~s(%esp)" (* 2 wordsize))
    (emit "  mov %esp, %eax")
    (emit "  add $~s, %eax"      (* 2 wordsize))
    (emit "  mov %eax, ~s(%esp)" (* 1 wordsize))
    (emit "  mov %ebp, ~s(%esp)" (* 0 wordsize))
    (emit "  mov %edi, ~s(%ebp)" edi-offset)
    (emit-call "heap_alloc")
    (emit-adjust-base (- new-si))
    (emit "  mov ~s(%ebp), %edi" edi-offset)))
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

(define-primitive (make-symbol si env str)
  (emit-expr si env str)
  (emit "  sub $~s, %eax" stringtag)
  (emit "  or $~s, %eax" symboltag))
(define-primitive (string-symbol si env symbol)
  (emit-expr si env symbol)
  (emit "  sub $~s, %eax" symboltag)
  (emit "  or $~s, %eax" stringtag))
(define-primitive (symbol? si env arg)
  (emit-object? symboltag si env arg))
(define-primitive (make-string si env length)
  (emit-expr-save si env length)
  (emit-make-string si))
(define (emit-make-string si)
  (emit "  shr $~s, %eax" fxshift)
  (emit "  add $~s, %eax" wordsize)
  (emit-heap-alloc-dynamic (next-stack-index si))
  (emit-stack-to-heap si 0)
  (emit "  or $~s, %eax" stringtag))
(define-primitive (string? si env arg)
  (emit-object? stringtag si env arg))
(define-primitive ($string-set! si env string index value)
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
(define-primitive ($string-ref si env string index)
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
  (emit-heap-alloc-dynamic (next-stack-index si))
  (emit-stack-to-heap si 0)
  (emit "  or $~s, %eax" vectortag))
(define-primitive (vector? si env arg)
  (emit-object? vectortag si env arg))
(define-primitive (vector-length si env arg)
  (emit-expr si env arg)
  (emit-heap-load (- vectortag)))
(define-primitive ($vector-set! si env vector index value)
  (emit-expr si env index)
  (emit "  add $~s, %eax" wordsize)
  (emit-stack-save si)
  (emit-expr-save (next-stack-index si) env value)
  (emit-expr si env vector)
  (emit "  add ~s(%esp), %eax" si)
  (emit-stack-to-heap (next-stack-index si) (- vectortag)))
(define-primitive ($vector-ref si env vector index)
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
  (emit-cons si (next-stack-index (next-stack-index si))))
(define (emit-cons si free-si)
  (emit-heap-alloc-static free-si pairsize)
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
  (emit "  mov %ecx, %esi")
  (emit "  mov 12(%esp), %ebp")
  (emit "  mov 8(%esp), %esp")
  (emit "  mov $0, %edi")
  (emit-call "L_scheme_entry")
  (emit "  mov %esi, %ecx")
  (restore-registers)
  (emit "  ret")
  (emit-labels (all-conversions expr) emit-scheme-entry))
