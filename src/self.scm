(define (cadr x) (car (cdr x)))
(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cddr x) (cdr (cdr x)))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadadr x) (car (cdr (car (cdr x)))))

(define props '())

(define (putprop s k v)
  (set! props (cons (cons (cons s k) v) props)))

(define (getprop s k)
  (let ((p (assoc (cons s k) props)))
    (and p (cdr p))))

(define (fprintf port format . args)
  (let loop ((i 0) (args args))
    (if (fx>= i (string-length format))
        'done
        (let ((c (string-ref format i)))
          (if (eq? c #\~)
              (begin
                (display (car args) port)
                (loop (+ i 2) (cdr args)))
              (begin
                (display c port)
                (loop (+ i 1) args)))))))
(define (printf format . args)
  (apply fprintf (current-output-port) format args))

(define (newline (port (current-output-port)))
  (display "\n" port))

(define (assert x)
  (unless x
    (error 'assert "assertion failed")))

(define (reverse xs)
  (define (iter xs acc)
    (if (null? xs)
        acc
        (iter (cdr xs) (cons (car xs) acc))))
  (iter xs '()))

(define (append1 xs ys)
  (if (not (pair? xs))
      ys
      (cons (car xs) (append1 (cdr xs) ys))))

(define (append . zss)
  (if (null? zss)
      '()
      (if (null? (cdr zss))
          (car zss)
          (append1 (car zss) (apply append (cdr zss))))))

(define (list? xs)
  (or (null? xs) (and (pair? xs) (list? (cdr xs)))))

(define (map1 f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map1 f (cdr xs)))))

(define (map f . xss)
  (if (null? (car xss))
      '()
      (cons (apply f (map1 car xss)) (apply map f (map1 cdr xss)))))

(define (filter p xs)
  (if (null? xs)
      '()
      (if (p (car xs))
          (cons (car xs) (filter p (cdr xs)))
          (filter p (cdr xs)))))

(define (for-each f . xss)
  (if (null? (car xss))
      'done
      (begin
        (apply f (map1 car xss))
        (apply for-each f (map1 cdr xss)))))

(define (equal? x y)
  (or (eq? x y)
      (cond
        ((pair? x)
         (and (pair? y)
              (equal? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((string? x)
         (and (string? y)
              (string=? x y)))
        (else #f))))

(define (member x xs)
  (if (null? xs)
      #f
      (if (equal? x (car xs))
          xs
          (member x (cdr xs)))))

(define (assoc k m)
  (if (null? m)
      #f
      (if (equal? k (car (car m)))
          (car m)
          (assoc k (cdr m)))))

(define (string->list s)
  (let loop ((i 0))
    (if (fx>= i (string-length s))
        '()
        (cons (string-ref s i) (loop (fxadd1 i))))))

(define (vector->list s)
  (let loop ((i 0))
    (if (fx>= i (vector-length s))
        '()
        (cons (vector-ref s i) (loop (fxadd1 i))))))

(define (string-append . args)
  (list->string (apply append (map string->list args))))

(define (substring s a b)
  (let ((r (make-string (- b a))))
    (let loop ((i 0))
      (if (= (fx+ a i) b)
          r
          (begin
            (string-set! r i (string-ref s (fx+ a i)))
            (loop (+ 1 i)))))))
(define (to-string x)
  (cond
    ((symbol? x)
     (symbol->string x))
    ((char? x)
     (string x))
    ((string? x)
     x)
    ((number? x)
     (number->string x))
    (else "TODO")))

(define (format x . args)
  (apply string-append (cons x (map to-string args))))

(define (ash x y)
  (s_ash x y))
(define (bitwise-ior x y)
  (s_bitwise_ior x y))
(define (expt b n)
  (if (fx= n 0)
      1
      (* b (expt b (fxsub1 n)))))
(define (make-parameter p f)
  (let ((r (f p)))
    (lambda () r)))
(define (void)
  0)

(define (add1 x) (fxadd1 x))
(define (sub1 x) (fxsub1 x))
(define (+ x y) (fx+ x y))
(define (- . args)
  (if (null? (cdr args))
      (fx- 0 (car args))
      (if (null? (cdr (cdr args)))
          (fx- (car args) (cadr args))
          (error 'fx- "one or two arguments expected"))))
(define * fx*)
(define div fxquotient)
(define < fx<)
(define <= fx<=)
(define >= fx>=)
(define > fx>)
(define = fx=)
(define number? fixnum?)
(define integer? fixnum?)
(define exact? fixnum?)
(define zero? fxzero?)
(define symbol->string string-symbol)
(define char->integer char->fixnum)
(define assv assoc)

(define (number->string n)
  (define (iter n acc)
    (if (= n 0)
        (if (null? acc)
            "0"
            (list->string acc))
        (iter (fxquotient n 10) (cons (fixnum->char (+ (char->fixnum #\0) (fxremainder n 10))) acc))))
  (iter n '()))
