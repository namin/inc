
#!eof

(add-tests-with-string-output "tokenizer"
  [(let ()
     (define test-tokenizer
       (lambda (p)
         ;(display (input-port? p) (standard-error-port))
         (let ([tok (read-token p)])
           (cond
            [(eof-object? tok) 'ok]
            [(or (eq? tok 'lparen)
                 (eq? tok 'rparen)
                 (eq? tok 'vparen)
                 (eq? tok 'lbrack)
                 (eq? tok 'rbrack)
                 (eq? tok 'dot)
                 (and (pair? tok)
                      (or (eq? (car tok) 'datum)
                          (eq? (car tok) 'macro))))
             (test-tokenizer p)]
            [else
             (display tok)
             (error 'test "Invalid token ~s" tok)]))))
     (define test-file
       (lambda (filename)
         (display "Testing " (standard-error-port))
         (display filename (standard-error-port))
         (display "..." (standard-error-port))
         (let ([p (open-input-file filename)])
         ;  (display (input-port? p)(standard-error-port))
           (test-tokenizer p))))
     (define test-files
       (lambda (files)
         (unless (null? files)
           (test-file (car files))
           (test-files (cdr files)))))
     (define filenames
       '("libsymboltable-3.3.ss"
         "libhandlers-3.3.ss"
         "libcore-4.3.ss"
         "libio-4.2.ss"
         "libwriter-4.1.ss"
         "libtokenizer-4.3.ss"
         "compiler-4.3.ss"))
     (when (null? filenames)
       (error 'no-files-provided-in-test "add them"))
     (test-files filenames)
     'ok) => "ok\n"]

)



(add-tests-with-string-output "reader"
  [(let ()
     (define test-reader
       (lambda (p)
         (let ([x (read p)])
           (cond
            [(eof-object? x) 'ok]
            [else (test-reader p)]))))
     (define test-file
       (lambda (filename)
         (display "Testing " (standard-error-port))
         (display filename (standard-error-port))
         (display "..." (standard-error-port))
         (test-reader (open-input-file filename))))
     (define test-files
       (lambda (files)
         (unless (null? files)
           (test-file (car files))
           (test-files (cdr files)))))
     (define filenames
       '("libsymboltable-3.3.ss"
         "libhandlers-3.3.ss"
         "libcore-4.3.ss"
         "libio-4.2.ss"
         "libwriter-4.1.ss"
         "libtokenizer-4.3.ss"
         "compiler-4.3.ss"))
     (when (null? filenames)
       (error 'no-files-provided-in-test "add them"))
     (test-files filenames)
     'ok) => "ok\n"]

)
