(add-tests-with-string-output "eof-object"
  [(eof-object? (eof-object)) => "#t\n"]

  [(null? (eof-object)) => "#f\n"]
  [(boolean? (eof-object)) => "#f\n"]
  [(string? (eof-object)) => "#f\n"]
  [(char? (eof-object)) => "#f\n"]
  [(pair? (eof-object)) => "#f\n"]
  [(symbol? (eof-object)) => "#f\n"]
  [(procedure? (eof-object)) => "#f\n"]
  [(vector? (eof-object)) => "#f\n"]
  [(not (eof-object)) => "#f\n"]

  [(eof-object? #\a) => "#f\n"]
  [(eof-object? #t) => "#f\n"]
  [(eof-object? 12) => "#f\n"]
  [(eof-object? '(1 2 3)) => "#f\n"]
  [(eof-object? '()) => "#f\n"]
  [(eof-object? '#(foo)) => "#f\n"]
  [(eof-object? (lambda (x) x)) => "#f\n"]
  [(eof-object? 'baz) => "#f\n"]
  )



(add-tests-with-string-output "read-char"
  [(begin
     (let ([p (open-output-file "inc.tmp" 'replace)])
       (display "Hello World!" p)
       (close-output-port p))
     (let ([p (open-input-file "inc.tmp")])
       (define loop
         (lambda ()
           (let ([x (read-char p)])
             (if (eof-object? x)
                 (begin
                   (close-input-port p)
                   '())
                 (begin
                   (display x)
                   (loop))))))
       (loop))
     (exit))
   => "Hello World!"]
  [(let ([s (make-string 10000)]
         [t "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz12344567890<>,./?;:'\"[]{}\\|`~!@#$%^&*()-_=+"])
     (define fill-string!
       (lambda (i j)
         (unless (fx= i (string-length s))
           (if (fx>= j (string-length t))
               (fill-string! i (fx- j (string-length t)))
               (begin
                 (string-set! s i (string-ref t j))
                 (fill-string! (fx+1 i) (fx+ j 17)))))))
     (define write-string!
       (lambda (i p)
         (cond
           [(fx= i (string-length s)) (close-output-port p)]
           [else
            (write-char (string-ref s i) p)
            (write-string! (fx+1 i) p)])))
     (define verify
       (lambda (i p)
         (let ([x (read-char p)])
           (cond
             [(eof-object? x)
              (close-input-port p)
              (fx= i (string-length s))]
             [(fx= i (string-length s)) (error 'verify "file too short")]
             [(char= (string-ref s i) x)
              (verify (fx+1 i) p)]
             [else (error 'verify "mismatch")]))))
     (fill-string! 0 0)
     (write-string! 0 (open-output-file "inc.tmp" 'replace))
     (verify 0 (open-input-file "inc.tmp"))) => "#t\n"]
)
