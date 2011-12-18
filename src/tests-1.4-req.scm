
(add-tests-with-string-output "if"
  [(if #t 12 13) => "12\n"]
  [(if #f 12 13) => "13\n"]
  [(if 0 12 13)  => "12\n"]
  [(if () 43 ()) => "43\n"]
  [(if #t (if 12 13 4) 17) => "13\n"]
  [(if #f 12 (if #f 13 4)) => "4\n"]
  [(if #\X (if 1 2 3) (if 4 5 6)) => "2\n"]
  [(if (not (boolean? #t)) 15 (boolean? #f)) => "#t\n"]
  [(if (if (char? #\a) (boolean? #\b) (fixnum? #\c)) 119 -23) => "-23\n"]
  [(if (if (if (not 1) (not 2) (not 3)) 4 5) 6 7) => "6\n"] 
  [(if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7) => "7\n"] 
  [(not (if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7)) => "#f\n"] 
  [(if (char? 12) 13 14) => "14\n"]
  [(if (char? #\a) 13 14) => "13\n"]
  [(fxadd1 (if (fxsub1 1) (fxsub1 13) 14)) => "13\n"]
)
