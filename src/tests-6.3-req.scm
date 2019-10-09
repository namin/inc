(add-tests-with-string-output-noboot "peek-char"
  [(let ()
     (load "self.scm")
     (load "reader.scm")
     (let ((f (open-input-file "reader.scm")))
       (peek-char f)))
   => "#\\;\n"]
  [(let ()
     (load "self.scm")
     (load "reader.scm")
     (let ((f (open-input-file "reader.scm")))
       (peek-char f)
       (read1-char f)
       (read1-char f)))
   => "#\\;\n"]
)

(add-tests-with-string-output-noboot "read-token"
  [(let ()
     (load "self.scm")
     (load "reader.scm")
     (let ((f (open-input-file "reader.scm")))
       (read-token f)
       (read-token f)))
   => "define\n"]
)

(add-tests-with-string-output-noboot "read"
  [(let ()
     (load "self.scm")
     (load "reader.scm")
     (let ((f (open-input-file "reader.scm")))
       (read f)
       (read f)
       (read f)
       (read f)
       (read f)
       (read f)))
   => "(define (char-numeric? c) (and (<= (char->fixnum #\\0) (char->fixnum c)) (<= (char->fixnum c) (char->fixnum #\\9))))\n"]
)
