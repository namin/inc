


(add-tests-with-string-output "fxadd1"
  [($fxadd1 0) => "1\n"]
  [($fxadd1 -1) => "0\n"]
  [($fxadd1 1) => "2\n"]
  [($fxadd1 -100) => "-99\n"]
  [($fxadd1 1000) => "1001\n"]
  [($fxadd1 536870910) => "536870911\n"]
  [($fxadd1 -536870912) => "-536870911\n"]
  [($fxadd1 ($fxadd1 0)) => "2\n"]
  [($fxadd1 ($fxadd1 ($fxadd1 ($fxadd1 ($fxadd1 ($fxadd1 12)))))) => "18\n"]
  )

(add-tests-with-string-output "fixnum->char and char->fixnum"
   [($fixnum->char 65) => "#\\A\n"]
   [($fixnum->char 97) => "#\\a\n"]
   [($fixnum->char 122) => "#\\z\n"]
   [($fixnum->char 90) => "#\\Z\n"]
   [($fixnum->char 48) => "#\\0\n"]
   [($fixnum->char 57) => "#\\9\n"]
   [($char->fixnum #\A) => "65\n"]
   [($char->fixnum #\a) => "97\n"]
   [($char->fixnum #\z) => "122\n"]
   [($char->fixnum #\Z) => "90\n"]
   [($char->fixnum #\0) => "48\n"]
   [($char->fixnum #\9) => "57\n"]
   [($char->fixnum ($fixnum->char 12)) => "12\n"]
   [($fixnum->char ($char->fixnum #\x)) => "#\\x\n"]
)

(add-tests-with-string-output "fixnum?"
   [(fixnum? 0) => "#t\n"]
   [(fixnum? 1) => "#t\n"]
   [(fixnum? -1) => "#t\n"]
   [(fixnum? 37287) => "#t\n"]
   [(fixnum? -23873) => "#t\n"]
   [(fixnum? 536870911) => "#t\n"]
   [(fixnum? -536870912) => "#t\n"]
   [(fixnum? #t) => "#f\n"]
   [(fixnum? #f) => "#f\n"]
   [(fixnum? ()) => "#f\n"]
   [(fixnum? #\Q) => "#f\n"]
   [(fixnum? (fixnum? 12)) => "#f\n"]
   [(fixnum? (fixnum? #f)) => "#f\n"]
   [(fixnum? (fixnum? #\A)) => "#f\n"]
   [(fixnum? ($char->fixnum #\r)) => "#t\n"]
   [(fixnum? ($fixnum->char 12)) => "#f\n"]
)


(add-tests-with-string-output "fxzero?"
   [($fxzero? 0) => "#t\n"]
   [($fxzero? 1) => "#f\n"]
   [($fxzero? -1) => "#f\n"]
)

(add-tests-with-string-output "null?"
   [(null? ()) => "#t\n"]
   [(null? #f) => "#f\n"]
   [(null? #t) => "#f\n"]
   [(null? (null? ())) => "#f\n"]
   [(null? #\a) => "#f\n"]
   [(null? 0) => "#f\n"]
   [(null? -10) => "#f\n"]
   [(null? 10) => "#f\n"]
)

(add-tests-with-string-output "boolean?"
   [(boolean? #t) => "#t\n"]
   [(boolean? #f) => "#t\n"]
   [(boolean? 0) => "#f\n"]
   [(boolean? 1) => "#f\n"]
   [(boolean? -1) => "#f\n"]
   [(boolean? ()) => "#f\n"]
   [(boolean? #\a) => "#f\n"]
   [(boolean? (boolean? 0)) => "#t\n"]
   [(boolean? (fixnum? (boolean? 0))) => "#t\n"]
)

(add-tests-with-string-output "char?"
   [(char? #\a) => "#t\n"]
   [(char? #\Z) => "#t\n"]
   [(char? #\newline) => "#t\n"]
   [(char? #t) => "#f\n"]
   [(char? #f) => "#f\n"]
   [(char? ()) => "#f\n"]
   [(char? (char? #t)) => "#f\n"]
   [(char? 0) => "#f\n"]
   [(char? 23870) => "#f\n"]
   [(char? -23789) => "#f\n"]
)

(add-tests-with-string-output "not"
  [(not #t) => "#f\n"]
  [(not #f) => "#t\n"]
  [(not 15) => "#f\n"]
  [(not ()) => "#f\n"]
  [(not #\A) => "#f\n"]
  [(not (not #t)) => "#t\n"]
  [(not (not #f)) => "#f\n"]
  [(not (not 15)) => "#t\n"]
  [(not (fixnum? 15)) => "#f\n"]
  [(not (fixnum? #f)) => "#t\n"]
)

(add-tests-with-string-output "fxlognot"
 [($fxlognot 0) => "-1\n"]
 [($fxlognot -1) => "0\n"]
 [($fxlognot 1) => "-2\n"]
 [($fxlognot -2) => "1\n"]
 [($fxlognot 536870911) => "-536870912\n"]
 [($fxlognot -536870912) => "536870911\n"]
 [($fxlognot ($fxlognot 237463)) => "237463\n"]
)

