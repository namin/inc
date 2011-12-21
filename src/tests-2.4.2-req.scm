(add-tests-with-string-output "and/or"
  [(and) => "#t\n"]
  [(and 5) => "5\n"]
  [(and #f) => "#f\n"]
  [(and 5 6) => "6\n"]
  [(and #f ((lambda (x) (x x)) (lambda (x) (x x)))) => "#f\n"]
  [(or) => "#f\n"]
  [(or #t) => "#t\n"]
  [(or 5) => "5\n"]
  [(or 1 2 3) => "1\n"]
  [(or (cons 1 2) ((lambda (x) (x x)) (lambda (x) (x x)))) => "(1 . 2)\n"]
  [(let ([if 12]) (or if 17)) => "12\n"]
  [(let ([if 12]) (and if 17)) => "17\n"]
  [(let ([let 8]) (or let 18)) => "8\n"]
  [(let ([let 8]) (and let 18)) => "18\n"]
  [(let ([t 1])
     (and (begin (set! t (fxadd1 t)) t) t)) => "2\n"]
  [(let ([t 1])
     (or (begin (set! t (fxadd1 t)) t) t)) => "2\n"]
)


(add-tests-with-string-output "when/unless"
  [(let ([x (cons 1 2)])
     (when (pair? x) 
       (set-car! x (fx+ (car x) (cdr x))))
     x) => "(3 . 2)\n"]
  [(let ([x (cons 1 2)])
     (when (pair? x) 
       (set-car! x (fx+ (car x) (cdr x)))
       (set-car! x (fx+ (car x) (cdr x))))
     x) => "(5 . 2)\n"]
  [(let ([x (cons 1 2)])
     (unless (fixnum? x) 
       (set-car! x (fx+ (car x) (cdr x))))
     x) => "(3 . 2)\n"]
  [(let ([x (cons 1 2)])
     (unless (fixnum? x) 
       (set-car! x (fx+ (car x) (cdr x)))
       (set-car! x (fx+ (car x) (cdr x))))
     x) => "(5 . 2)\n"]
  [(let ([let 12])
     (when let let let let let)) => "12\n"]
  [(let ([let #f])
     (unless let let let let let)) => "#f\n"]
  )