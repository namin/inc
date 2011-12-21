(add-tests-with-string-output "letrec"
  [(letrec () 12) => "12\n"]
  [(letrec ([f 12]) f) => "12\n"]
  [(letrec ([f 12] [g 13]) (fx+ f g)) => "25\n"]
  [(letrec ([fact
             (lambda (n)
               (if (fxzero? n)
                   1
                   (fx* n (fact (fxsub1 n)))))])
    (fact 5)) => "120\n"]
  [(letrec ([f 12] [g (lambda () f)])
     (g)) => "12\n"]
  [(letrec ([f 12] [g (lambda (n) (set! f n))])
    (g 130)
    f) => "130\n"]
  [(letrec ([f (lambda (g) (set! f g) (f))])
     (f (lambda () 12))) => "12\n"]
  [(letrec ([f (cons (lambda () f)
                     (lambda (x) (set! f x)))])
    (let ([g (car f)])
      ((cdr f) 100)
      (g))) => "100\n"]
  [(letrec ([f (letrec ([g (lambda (x) (fx* x 2))])
                  (lambda (n) (g (fx* n 2))))])
      (f 12)) => "48\n"]
  [(letrec ([f (lambda (f n)
                  (if (fxzero? n)
                      1
                      (fx* n (f f (fxsub1 n)))))])
      (f f 5)) => "120\n"]
  [(let ([f (lambda (f)
              (lambda (n)
                 (if (fxzero? n)
                     1
                     (fx* n (f (fxsub1 n))))))])
     (letrec ([fix
               (lambda (f)
                 (f (lambda (n) ((fix f) n))))])
      ((fix f) 5))) => "120\n"]
)

(add-tests-with-string-output "letrec*"
  [(letrec* () 12) => "12\n"]
  [(letrec* ([f 12]) f) => "12\n"]
  [(letrec* ([f 12] [g 13]) (fx+ f g)) => "25\n"]
  [(letrec* ([fact
             (lambda (n)
               (if (fxzero? n)
                   1
                   (fx* n (fact (fxsub1 n)))))])
    (fact 5)) => "120\n"]
  [(letrec* ([f 12] [g (lambda () f)])
     (g)) => "12\n"]
  [(letrec* ([f 12] [g (lambda (n) (set! f n))])
    (g 130)
    f) => "130\n"]
  [(letrec* ([f (lambda (g) (set! f g) (f))])
     (f (lambda () 12))) => "12\n"]
  [(letrec* ([f (cons (lambda () f)
                      (lambda (x) (set! f x)))])
    (let ([g (car f)])
      ((cdr f) 100)
      (g))) => "100\n"]
  [(letrec* ([f (letrec* ([g (lambda (x) (fx* x 2))])
                   (lambda (n) (g (fx* n 2))))])
      (f 12)) => "48\n"]
  [(letrec* ([f (lambda (f n)
                   (if (fxzero? n)
                       1
                       (fx* n (f f (fxsub1 n)))))])
      (f f 5)) => "120\n"]
  [(let ([f (lambda (f)
              (lambda (n)
                 (if (fxzero? n)
                     1
                     (fx* n (f (fxsub1 n))))))])
     (letrec* ([fix
                (lambda (f)
                  (f (lambda (n) ((fix f) n))))])
      ((fix f) 5))) => "120\n"]
  [(letrec* ([a 12] [b (fx+ a 5)] [c (fx+ b a)])
      c) => "29\n"]
)
