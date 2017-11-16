
(add-tests-with-string-output "symbols"
 [(symbol? 'foo) => "#t"]
 [(symbol? '()) => "#f"]
 [(symbol? "") => "#f"]
 [(symbol? '(1 2)) => "#f"]
 [(symbol? '#()) => "#f"]
 [(symbol? (lambda (x) x)) => "#f"]
 [(symbol? 'foo) => "#t"]
 [(string? 'foo) => "#f"]
 [(pair? 'foo) => "#f"]
 [(vector? 'foo) => "#f"]
 [(null? 'foo) => "#f"]
 [(boolean? 'foo) => "#f"]
 [(procedure? 'foo) => "#f"]
 [(eq? 'foo 'bar) => "#f"]
 [(eq? 'foo 'foo) => "#t"]
 ['foo => "foo"]
 ['(foo bar baz) => "(foo bar baz)"]
 ['(foo foo foo foo foo foo foo foo foo foo foo)
  => "(foo foo foo foo foo foo foo foo foo foo foo)"]

)
