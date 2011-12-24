(define-lib-primitive (length lst)
  (if (null? lst)
      0
      (fxadd1 (length (cdr lst)))))

(define-lib-primitive (fill args setop)
  (letrec ([rec (lambda (index args)
                  (unless (null? args)
                    (setop index (car args))
                    (rec (fxadd1 index) (cdr args))))])
    (rec 0 args)))
             
(define-lib-primitive (vector . args)
  (let ([v (make-vector (length args))])
    (fill args (lambda (index arg) (vector-set! v index arg)))
    v))

(define-lib-primitive (string . args)
  (let ([s (make-string (length args))])
    (fill args (lambda (index arg) (string-set! s index arg)))
    s))

(define-lib-primitive (list . args)
  args)

(define-lib-primitive (make_cell value)
  (cons value '()))

(define-lib-primitive (cell_get cell)
  (car cell))

(define-lib-primitive (cell_set cell value)
  (set-car! cell value))

(define-lib-primitive __symbols__
  (make_cell '()))

(define-lib-primitive (string_equals s1 s2)
  (letrec ([rec (lambda (index)
                  (or (fx= index (string-length s1))
                      (and (char= (string-ref s1 index) (string-ref s2 index))
                           (rec (fxadd1 index)))))])
    (and (string? s1) (string? s2) (fx= (string-length s1) (string-length s2)) (rec 0))))
        
(define-lib-primitive (__find_symbol__ str)
  (letrec ([rec (lambda (symbols)
                  (cond
                   [(null? symbols) #f]
                   [(string_equals str (string-symbol (car symbols))) (car symbols)]
                   [else (rec (cdr symbols))]))])
    (rec (cell_get __symbols__))))

(define-lib-primitive (string->symbol str)
  (or (__find_symbol__ str)
      (let ([symbol (make-symbol str)])
        (cell_set __symbols__ (cons symbol (cell_get __symbols__)))
        symbol)))

(define-lib-primitive (error . args)
  ;; stub
  args)