
(add-tests-with-string-output "symbols"
 [(symbol? 'foo) => "#t\n"]
 [(symbol? '()) => "#f\n"]
 [(symbol? "") => "#f\n"]
 [(symbol? '(1 2)) => "#f\n"]
 [(symbol? '#()) => "#f\n"]
 [(symbol? (lambda (x) x)) => "#f\n"]
 [(symbol? 'foo) => "#t\n"]
 [(string? 'foo) => "#f\n"]
 [(pair? 'foo) => "#f\n"]
 [(vector? 'foo) => "#f\n"]
 [(null? 'foo) => "#f\n"]
 [(boolean? 'foo) => "#f\n"]
 [(procedure? 'foo) => "#f\n"]
 [(eq? 'foo 'bar) => "#f\n"]
 [(eq? 'foo 'foo) => "#t\n"]
 ['foo => "foo\n"]
 ['(foo bar baz) => "(foo bar baz)\n"]
 ['(foo foo foo foo foo foo foo foo foo foo foo) 
  => "(foo foo foo foo foo foo foo foo foo foo foo)\n"]
 
)
