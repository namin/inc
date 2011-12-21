(add-tests-with-string-output "cond"
  [(cond [1 2] [else 3]) => "2\n"]
  [(cond [1] [else 13]) => "1\n"]
  [(cond [#f #t] [#t #f]) => "#f\n"]
  [(cond [else 17]) => "17\n"]
  [(cond [#f] [#f 12] [12 13]) => "13\n"]
  [(cond [(cons 1 2) => (lambda (x) (cdr x))]) => "2\n"]
  [(let ([else #t])
     (cond
      [else 1287])) => "1287\n"]
  [(let ([else 17])
     (cond
      [else])) => "17\n"]
  [(let ([else 17])
     (cond
      [else => (lambda (x) x)])) => "17\n"]
  [(let ([else #f])
     (cond 
       [else ((lambda (x) (x x)) (lambda (x) (x x)))])
     else) => "#f\n"]
  [(let ([=> 12])
    (cond
     [12 => 14]
     [else 17])) => "14\n"]
  [(let ([=> 12])
    (cond
      [=>])) => "12\n"]
  [(let ([=> 12])
    (cond
      [=> =>])) => "12\n"]
  [(let ([=> 12])
    (cond
      [=> => =>])) => "12\n"]
  [(let ([let 12])
    (cond
      [let => (lambda (x) (fx+ let x))]
      [else 14])) => "24\n"]
)

(load "tests-2.4.2-req.scm")
(load "tests-2.4.1-req.scm")