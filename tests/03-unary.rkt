#lang racket

(require "../src/driver.rkt")

(add-tests "inc"
  [(inc 0) => 1]
  [(inc -1) => 0]
  [(inc 1) => 2]
  [(inc -100) => -99]
  [(inc 1000) => 1001]
  [(inc 536870910) => 536870911]
  [(inc -536870912) => -536870911]
  [(inc (inc 0)) => 2]
  [(inc (inc (inc (inc (inc (inc 12)))))) => 18])

(add-tests "fixnum->char and char->fixnum"
   [(fixnum->char 65) => #\A]
   [(fixnum->char 97) => #\a]
   [(fixnum->char 122) => #\z]
   [(fixnum->char 90) => #\Z]
   [(fixnum->char 48) => #\0]
   [(fixnum->char 57) => #\9]
   [(char->fixnum #\A) => 65]
   [(char->fixnum #\a) => 97]
   [(char->fixnum #\z) => 122]
   [(char->fixnum #\Z) => 90]
   [(char->fixnum #\0) => 48]
   [(char->fixnum #\9) => 57]
   [(char->fixnum (fixnum->char 12)) => 12]
   [(fixnum->char (char->fixnum #\x)) => #\x])

(add-tests "fixnum?"
   [(fixnum? 0) => #t]
   [(fixnum? 1) => #t]
   [(fixnum? -1) => #t]
   [(fixnum? 37287) => #t]
   [(fixnum? -23873) => #t]
   [(fixnum? 536870911) => #t]
   [(fixnum? -536870912) => #t]
   [(fixnum? #t) => #f]
   [(fixnum? #f) => #f]
   [(fixnum? ()) => #f]
   [(fixnum? #\Q) => #f]
   [(fixnum? (fixnum? 12)) => #f]
   [(fixnum? (fixnum? #f)) => #f]
   [(fixnum? (fixnum? #\A)) => #f]
   [(fixnum? (char->fixnum #\r)) => #t]
   [(fixnum? (fixnum->char 12)) => #f])

(add-tests "zero?"
   [(zero? 0) => #t]
   [(zero? 1) => #f]
   [(zero? -1) => #f])

(add-tests "null?"
   [(null? ()) => #t]
   [(null? #f) => #f]
   [(null? #t) => #f]
   [(null? (null? ())) => #f]
   [(null? #\a) => #f]
   [(null? 0) => #f]
   [(null? -10) => #f]
   [(null? 10) => #f])

(add-tests "boolean?"
   [(boolean? #t) => #t]
   [(boolean? #f) => #t]
   [(boolean? 0) => #f]
   [(boolean? 1) => #f]
   [(boolean? -1) => #f]
   [(boolean? ()) => #f]
   [(boolean? #\a) => #f]
   [(boolean? (boolean? 0)) => #t]
   [(boolean? (fixnum? (boolean? 0))) => #t])

(add-tests "char?"
   [(char? #\a) => #t]
   [(char? #\Z) => #t]
   [(char? #\newline) => #t]
   [(char? #t) => #f]
   [(char? #f) => #f]
   [(char? ()) => #f]
   [(char? (char? #t)) => #f]
   [(char? 0) => #f]
   [(char? 23870) => #f]
   [(char? -23789) => #f])

(add-tests "not"
  [(not #t) => #f]
  [(not #f) => #t]
  [(not 15) => #f]
  [(not ()) => #f]
  [(not #\A) => #f]
  [(not (not #t)) => #t]
  [(not (not #f)) => #f]
  [(not (not 15)) => #t]
  [(not (fixnum? 15)) => #f]
  [(not (fixnum? #f)) => #t])

(add-tests "lognot"
  [(lognot (logor (lognot 7) (lognot 2))) => 2]
  [(lognot (logor (lognot 7) (lognot 2))) => 2]
  [(lognot (logor (lognot 7) 1)) => 6]
  [(lognot (logor (lognot 7) 1)) => 6]
  [(lognot -7) => 6])
