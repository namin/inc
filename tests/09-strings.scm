(add-tests "strings"
  ["Hello world !!" => "Hello world !!"]
  ["#t" => "#t"]
  ["1857" => "1857"]

  [(boolean? "hello") => #f]
  [(null? "hello") => #f]
  [(pair? "hello") => #f]
  [(string? "test") => #t]
  [(string? #f) => #f]
  [(string? #t) => #f]
  [(string? ()) => #f]
  [(string? (cons 1 2)) => #f]
  [(string? 1287) => #f]

  [(make-string 0) => ""]
  [(null? (make-string 4)) => #f]
  [(string? (make-string 0)) => #t]
  [(string? (make-string 4)) => #t])
