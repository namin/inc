(add-tests-with-string-output "strings"
  [(string? (make-string 0)) => "#t\n"]
  [(make-string 0) => "\"\"\n"]
  [(let ([s (make-string 1)]) 
     (string-set! s 0 #\a)
     (string-ref s 0)) => "#\\a\n"]
  [(let ([s (make-string 2)]) 
     (string-set! s 0 #\a)
     (string-set! s 1 #\b)
     (cons (string-ref s 0) (string-ref s 1))) => "(#\\a . #\\b)\n"]
  [(let ([i 0])
    (let ([s (make-string 1)]) 
     (string-set! s i #\a)
     (string-ref s i))) => "#\\a\n"]
  [(let ([i 0] [j 1])
    (let ([s (make-string 2)]) 
     (string-set! s i #\a)
     (string-set! s j #\b)
     (cons (string-ref s i) (string-ref s j)))) => "(#\\a . #\\b)\n"]
  [(let ([i 0] [c #\a])
    (let ([s (make-string 1)]) 
     (string-set! s i c)
     (string-ref s i))) => "#\\a\n"]
  [(string-length (make-string 12)) => "12\n"]
  [(string? (make-vector 12)) => "#f\n"]
  [(string? (cons 1 2)) => "#f\n"]
  [(string? 1287) => "#f\n"]
  [(string? ()) => "#f\n"]
  [(string? #t) => "#f\n"]
  [(string? #f) => "#f\n"]
  [(pair? (make-string 12)) => "#f\n"]
  [(null? (make-string 12)) => "#f\n"]
  [(boolean? (make-string 12)) => "#f\n"]
  [(vector? (make-string 12)) => "#f\n"]
  [(make-string 0) => "\"\"\n"]
  [(let ([v (make-string 2)])
     (string-set! v 0 #\t)
     (string-set! v 1 #\f)
     v) => "\"tf\"\n"]
  [(let ([v (make-string 2)])
     (string-set! v 0 #\x)
     (string-set! v 1 #\x)
     (char= (string-ref v 0) (string-ref v 1))) => "#t\n"]
  [(let ([v0 (make-string 3)])
     (let ([v1 (make-string 3)])
       (string-set! v0 0 #\a)
       (string-set! v0 1 #\b)
       (string-set! v0 2 #\c)
       (string-set! v1 0 #\d)
       (string-set! v1 1 #\e)
       (string-set! v1 2 #\f)
       (cons v0 v1))) => "(\"abc\" . \"def\")\n"]
  [(let ([n 2])
    (let ([v0 (make-string n)])
     (let ([v1 (make-string n)])
       (string-set! v0 0 #\a)
       (string-set! v0 1 #\b)
       (string-set! v1 0 #\c)
       (string-set! v1 1 #\d)
       (cons v0 v1)))) => "(\"ab\" . \"cd\")\n"]
  [(let ([n 3])
    (let ([v0 (make-string n)])
     (let ([v1 (make-string (string-length v0))])
       (string-set! v0 (fx- (string-length v0) 3) #\a)
       (string-set! v0 (fx- (string-length v1) 2) #\b)
       (string-set! v0 (fx- (string-length v0) 1) #\c)
       (string-set! v1 (fx- (string-length v1) 3) #\Z)
       (string-set! v1 (fx- (string-length v0) 2) #\Y)
       (string-set! v1 (fx- (string-length v1) 1) #\X)
       (cons v0 v1)))) =>  "(\"abc\" . \"ZYX\")\n"]
  [(let ([n 1])
     (string-set! (make-string n) (fxsub1 n) (fixnum->char 34))
     n) => "1\n"]
  [(let ([n 1])
     (let ([v (make-string 1)])
       (string-set! v (fxsub1 n) (fixnum->char n))
       (char->fixnum (string-ref v (fxsub1 n))))) => "1\n"]
 [(let ([v0 (make-string 1)])
    (string-set! v0 0 #\a)
    (let ([v1 (make-string 1)])
      (string-set! v1 0 #\A)
      (string-set! (if (string? v0) v0 v1) 
           (fxsub1 (string-length (if (string? v0) v0 v1)))
           (fixnum->char
             (fxadd1 
                (char->fixnum
                  (string-ref
                     (if (string? v0) v0 v1)
                     (fxsub1 (string-length (if (string? v0) v0 v1))))))))
      (cons v0 v1))) => "(\"b\" . \"A\")\n"]
 [(let ([s (make-string 1)])
     (string-set! s 0 #\")
     s) => "\"\\\"\"\n"]
 [(let ([s (make-string 1)])
     (string-set! s 0 #\\)
     s) => "\"\\\\\"\n"]
)
