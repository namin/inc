(define (cps-top expr)
  (T-k expr (lambda (x) x)))

(define (aexpr? expr)
  (or (lambda? expr)
      (immediate? expr)
      (symbol? expr)
      (string? expr)
      (aexpr-primcall? expr)))

(define (T-k expr k)
  (cond
   [(aexpr? expr)
    (k (M expr))]
   [(begin? expr)
    (let ([expr (first (begin-seq expr))]
	  [exprs (rest (begin-seq expr))])
      (if (null? exprs)
	        (T-k expr k)
	        (T-k expr (lambda (_)
		                  (T-k (cons 'begin exprs) k)))))]
   [(if? expr)
    (let* ([exprc (if-test expr)]
	   [exprt (if-conseq expr)]
	   [exprf (if-altern expr)]
	   [$rv (unique-name '$rv)]
	   [cont (list 'lambda (list $rv) (k $rv))])
      (T-k exprc (lambda (aexp)
		               (list 'if aexp
			                   (T-c exprt cont)
			                   (T-c exprf cont)))))]
   [(let? expr)
    (let ([vars (map lhs (let-bindings expr))]
	  [vals (map rhs (let-bindings expr))])
      (T*-k vals (lambda ($vals)
		   (make-let
		    'let
		    (map bind vars $vals)
		    (T-k (let-body expr) k)))))]
   [(app? expr)
    (let* ([$rv (unique-name '$rv)]
	   [cont (list 'lambda (list $rv) (k $rv))])
      (T-c expr cont))]
   [else (error 'T-k (format "~s is not an expression" expr))]))


(define (T-c expr c)
  (cond
   [(aexpr? expr)
    (list c (M expr))]
   [(begin? expr)
    (let ([expr (first (begin-seq expr))]
	        [exprs (rest (begin-seq expr))])
      (if (null? exprs)
	        (T-c expr c)
	        (T-k expr (lambda (_)
		                  (T-c (cons 'begin exprs) c)))))]
   [(if? expr)
    (let ([exprc (if-test expr)]
	        [exprt (if-conseq expr)]
	        [exprf (if-altern expr)]
	        [$k (unique-name '$k)])
      (list (list 'lambda (list $k)
	                (T-k exprc (lambda (aexp)
			                         (list 'if aexp
			                               (T-c exprt $k)
			                               (T-c exprf $k)))))
	          c))]
   [(let? expr)
    (let ([vars (map lhs (let-bindings expr))]
	  [vals (map rhs (let-bindings expr))])
      (T*-k vals (lambda ($vals)
		   (make-let
		    'let
		    (map bind vars $vals)
		    (T-c (let-body expr) c)))))]
   [(app? expr)
    (let ([f (call-target expr)]
	  [es (call-args expr)])
      (T-k f (lambda ($f)
	       (T*-k es (lambda ($es)
			              (let ([app (cons $f (cons c $es))])
			                (if (call-apply? expr)
				                  (cons 'apply app)
				                  app)))))))]
   [else (error 'T-c (format "~s is not an expression" expr))]))

(define (T*-k exprs k)
  (cond
   [(null? exprs)
    (k '())]
   [(pair? exprs)
    (T-k (car exprs) (lambda (hd)
      (T*-k (cdr exprs) (lambda (tl)
        (k (cons hd tl))))))]))

(define (M aexpr)
  (cond
   [(lambda? aexpr)
    (let ([$k (unique-name '$k)])
      (list 'lambda (cons $k (lambda-formals aexpr))
	          (T-c (lambda-body aexpr) $k)))]
   [(eq? 'call/cc aexpr)
    '(lambda (cc f) (f cc (lambda (_ x) (cc x))))]
   [else aexpr]))

(set! cps-conversion
      (lambda (expr)
        (make-let
         'labels
         (let-bindings expr)
         (cps-top (let-body expr)))))
