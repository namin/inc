
(add-tests-with-string-output "string-set! errors"
  ; first with a fixed index
;
 [(let ((t 1)) 
    (and (begin (set! t (fxadd1 t)) t) 
         t)) => "2\n"]

 [(let ((f (if (boolean? (lambda () 12))
               (lambda () 13) 
               (lambda () 14)))) 
    (f)) => "14\n"] 

  [(let ([f 12])
     (let ([g (lambda () f)])
       (g))) => "12\n"]
  [(fx< 1 2) => "#t\n"]
  [(let ([f (lambda (x y) (fx< x y))])
     (f 10 10)) => "#f\n"]
  [(fx< 10 10) => "#f\n"]
  [(fx< 10 2) => "#f\n"]
  [(fx<= 1 2) => "#t\n"]
  [(fx<= 10 10) => "#t\n"]
  [(fx<= 10 2) => "#f\n"]
  #;[(let ([f
      (lambda (s i c) 
    (unless (string? s) 
      (error 'string-set!1 "not a string ~s" s))
    (unless (fixnum? i)
      (error 'string-set!2 "invalid index ~s" i))
    (if (fx< i ($string-length s))
        #f
        (error 's1 ""))
    (unless (fx>= i 0) 
      (error 'string-set!3 "index ~s is out of range for ~s" i s))
    (unless (and (fx< i (string-length s))
                 (fx>= i 0))
      (error 'string-set!3 "index ~s is out of range for ~s" i s))
    (unless (char? c)
      (error 'string-set!4 "not a char ~s" c))
    ($string-set! s i c) 12)])
   (let ([x ($string #\a #\b #\c)]
         [y #\a])
     (f x 8 y))) => ""]
     
  [(let ([x 12])
     (string-set! x 0 #\a)) => ""]
  [(let ([x (string #\a #\b #\c)]
         [y 12])
     (string-set! x 0 y)) => ""]
  [(let ([x (string #\a #\b #\c)]
         [y 12])
     (string-set! x 8 y)) => ""]
  [(let ([x (string #\a #\b #\c)]
         [y #\a])
     (string-set! x 8 y)) => ""]
  [(let ([x (string #\a #\b #\c)])
     (string-set! x 8 #\a)) => ""]
  [(let ([x (string #\a #\b #\c)]
         [y #\a])
     (string-set! x -1 y)) => ""]
 ; next the general case
 ;;; 6 kinds of errors:
 ;;;   string is either: 
 ;;;     lex-non-string, run-non-string, lex-string, valid
 ;;;   index is either:
 ;;;     lex-invalid, runtime-non-fixnum, runtime-above, runtime-below, valid
 ;;;   char is either:
 ;;;     lex-invalid, runtime-non-char, valid.
 ;;;  that's 4x5x3 = 60 tests!
 ;;;  If we skip over the lexical string check, (since I don't do it),
 ;;;  we have: 2x5x3 = 30 tests.
 
 [(let ([s (string #\a #\b #\c)] [i 1] [c #\X]) (string-set! s i c) s) 
  => "\"aXc\"\n"]
 [(let ([s (string #\a #\b #\c)] [i 1]) (string-set! s i #\X) s) 
  => "\"aXc\"\n"]
 [(let ([s (string #\a #\b #\c)] [i 1] [c 'X]) (string-set! s i c) s) 
  => ""]

 [(let ([s (string #\a #\b #\c)] [i 1] [c #\X]) (string-set! s 1 c) s) 
  => "\"aXc\"\n"]
 [(let ([s (string #\a #\b #\c)] [i 1]) (string-set! s 1 #\X) s) 
  => "\"aXc\"\n"]
 [(let ([s (string #\a #\b #\c)] [i 1] [c 'X]) (string-set! s 1 c) s) 
  => ""]
 
 [(let ([s (string #\a #\b #\c)] [i 3] [c #\X]) (string-set! s i c) s) 
  => ""]
 [(let ([s (string #\a #\b #\c)] [i 3]) (string-set! s i #\X) s) 
  => ""]
 [(let ([s (string #\a #\b #\c)] [i 3] [c 'X]) (string-set! s i c) s) 
  => ""]
 
 [(let ([s (string #\a #\b #\c)] [i -10] [c #\X]) (string-set! s i c) s) 
  => ""]
 [(let ([s (string #\a #\b #\c)] [i -11]) (string-set! s i #\X) s) 
  => ""]
 [(let ([s (string #\a #\b #\c)] [i -1] [c 'X]) (string-set! s i c) s) 
  => ""]
 
 [(let ([s (string #\a #\b #\c)] [i 'foo] [c #\X]) (string-set! s i c) s) 
  => ""]
 [(let ([s (string #\a #\b #\c)] [i 'foo]) (string-set! s i #\X) s) 
  => ""]
 [(let ([s (string #\a #\b #\c)] [i 'foo] [c 'X]) (string-set! s i c) s) 
  => ""]

 
 
 [(let ([s '(string #\a #\b #\c)] [i 1] [c #\X]) (string-set! s i c) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i 1]) (string-set! s i #\X) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i 1] [c 'X]) (string-set! s i c) s) 
  => ""]

 [(let ([s '(string #\a #\b #\c)] [i 1] [c #\X]) (string-set! s 1 c) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i 1]) (string-set! s 1 #\X) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i 1] [c 'X]) (string-set! s 1 c) s) 
  => ""]
 
 [(let ([s '(string #\a #\b #\c)] [i 3] [c #\X]) (string-set! s i c) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i 3]) (string-set! s i #\X) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i 3] [c 'X]) (string-set! s i c) s) 
  => ""]
 
 [(let ([s '(string #\a #\b #\c)] [i -10] [c #\X]) (string-set! s i c) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i -11]) (string-set! s i #\X) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i -1] [c 'X]) (string-set! s i c) s) 
  => ""]
 
 [(let ([s '(string #\a #\b #\c)] [i 'foo] [c #\X]) (string-set! s i c) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i 'foo]) (string-set! s i #\X) s) 
  => ""]
 [(let ([s '(string #\a #\b #\c)] [i 'foo] [c 'X]) (string-set! s i c) s) 
  => ""]
)

#!eof  
 
(add-tests-with-string-output "string errors"
  [(let ([f (lambda (a b c) (string a b c))])
     (f #\a #\b #\c)) => "\"abc\"\n"]
  [(let ([f (lambda (a b c) (string a b c))])
     (f #\a 12 #\c)) => ""]
  [(let ([f string])
     (f #\a #\b #\c)) => "\"abc\"\n"]
  [(let ([f string])
     (f #\a #\b 'x)) =>  ""]
  [(string #\a #\b #\c) => "\"abc\"\n"] 
  [(string #\a #\b #t) => ""] 
)
