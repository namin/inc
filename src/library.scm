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
