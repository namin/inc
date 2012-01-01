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
