;;; one possible implementation strategy for procedures is via closure
;;; conversion.

;;; Lambda does many things at the same time:
;;; 1) It creates a procedure object (ie. one that passes procedure?)
;;; 2) It contains both code (what to do when applied) and data (what
;;;    variables it references.
;;; 3) The procedure object, in addition to passing procedure?, can be
;;;    applied to arguments.

;;; First step: separate code from data:
;;; convert every program containing lambda to a program containing
;;; codes and closures:
;;; (let ([f (lambda () 12)]) (procedure? f))
;;; =>
;;; (codes ([f-code (code () () 12)])
;;;   (let ([f (closure f-code)])
;;;     (procedure? f)))
;;;
;;; The codes binds code names to code points.  Every code
;;; is of the form (code (formals ...) (free-vars ...) body)
;;;
;;; sexpr
;;; => recordize
;;; recognize lambda forms and applications
;;; =>
;;; (let ([y 12])
;;;   (let ([f (lambda (x) (fx+ y x))])
;;;     (fx+ (f 10) (f 0))))
;;; => convert closures
;;; (let ([y 12])
;;;   (let ([f (closure (code (x) (y) (fx+ x y)) y)])
;;;     (fx+ (call f 10) (call f 0))
;;; => lift codes
;;; (codes ([code0 (code (x) (y) (fx+ x y))])
;;;   (let ([y 12])
;;;     (let ([f (closure code0 y)])
;;;       (fx+ (call f 10) (call f 0)))))
;;; => code generation
;;; 1) codes form generates unique-labels for every code and
;;;    binds the names of the code to these labels.
;;; 2) Every code object has a list of formals and a list of free vars.
;;;    The formals are at stack locations -4(%esp), -8(%esp), -12(%esp), ...
;;;    The free vars are at -2(%edi), 2(%edi), 6(%edi), 10(%edi) ...
;;;    These are inserted in the environment and then the body of the code
;;;    is generated.
;;; 3) A (closure code-name free-vars ...) is generated the same way a
;;;    (vector val* ...) is generated:  First, the code-label and the free
;;;    variables are placed at 0(%ebp), 4(%ebp), 8(%ebp), etc..
;;;    A closure pointer is placed in %eax, and %ebp is incremented to the
;;;    next boundary.
;;; 4) A (call f arg* ...) does the following:
;;;    a) evaluates the args and places them at contiguous stack locations
;;;       si-8(%esp), si-12(%esp), ... (leaving room for two values).
;;;    b) The value of the current closure pointer, %edi, is saved on the
;;;       stack at si(%esp).
;;;    c) The closure pointer of the callee is loaded in %edi.
;;;    d) The value of %esp is adjusted by si
;;;    e) An indirect call to -6(%edi) is issued.
;;;    f) After return, the value of %esp is adjusted back by -si
;;;    g) The value of the closure pointer is restored.
;;;    The returned value is still in %eax.

(add-tests-with-string-output "procedure?"
  [(procedure? (lambda (x) x)) => "#t"]
  [(let ([f (lambda (x) x)]) (procedure? f)) => "#t"]
  [(procedure? (make-vector 0)) => "#f"]
  [(procedure? (make-string 0)) => "#f"]
  [(procedure? (cons 1 2)) => "#f"]
  [(procedure? #\S) => "#f"]
  [(procedure? ()) => "#f"]
  [(procedure? #t) => "#f"]
  [(procedure? #f) => "#f"]
  [(string? (lambda (x) x)) => "#f"]
  [(vector? (lambda (x) x)) => "#f"]
  [(boolean? (lambda (x) x)) => "#f"]
  [(null? (lambda (x) x)) => "#f"]
  [(not (lambda (x) x)) => "#f"]
)


(add-tests-with-string-output "applying thunks"
  [(let ([f (lambda () 12)]) (f)) => "12"]
  [(let ([f (lambda () (fx+ 12 13))]) (f)) => "25"]
  [(let ([f (lambda () 13)]) (fx+ (f) (f))) => "26"]
  [(let ([f (lambda ()
              (let ([g (lambda () (fx+ 2 3))])
                (fx* (g) (g))))])
    (fx+ (f) (f))) => "50"]
  [(let ([f (lambda ()
              (let ([f (lambda () (fx+ 2 3))])
                (fx* (f) (f))))])
    (fx+ (f) (f))) => "50"]
  [(let ([f (if (boolean? (lambda () 12))
                (lambda () 13)
                (lambda () 14))])
     (f)) => "14"]
)


(add-tests-with-string-output "parameter passing"
 [(let ([f (lambda (x) x)]) (f 12)) => "12"]
 [(let ([f (lambda (x y) (fx+ x y))]) (f 12 13)) => "25"]
 [(let ([f (lambda (x)
             (let ([g (lambda (x y) (fx+ x y))])
               (g x 100)))])
   (f 1000)) => "1100"]
 [(let ([f (lambda (g) (g 2 13))])
    (f (lambda (n m) (fx* n m)))) => "26"]
 [(let ([f (lambda (g) (fx+ (g 10) (g 100)))])
   (f (lambda (x) (fx* x x)))) => "10100"]
 [(let ([f (lambda (f n m)
             (if (fxzero? n)
                 m
                 (f f (fx-1 n) (fx* n m))))])
   (f f 5 1)) => "120"]
 [(let ([f (lambda (f n)
             (if (fxzero? n)
                 1
                 (fx* n (f f (fx-1 n)))))])
   (f f 5)) => "120"]
)


(add-tests-with-string-output "closures"
 [(let ([n 12])
    (let ([f (lambda () n)])
      (f))) => "12"]
 [(let ([n 12])
    (let ([f (lambda (m) (fx+ n m))])
      (f 100))) => "112"]
 [(let ([f (lambda (f n m)
             (if (fxzero? n)
                 m
                 (f (fx-1 n) (fx* n m))))])
   (let ([g (lambda (g n m) (f (lambda (n m) (g g n m)) n m))])
     (g g 5 1))) => "120"]
 [(let ([f (lambda (f n)
             (if (fxzero? n)
                 1
                 (fx* n (f (fx-1 n)))))])
   (let ([g (lambda (g n) (f (lambda (n) (g g n)) n))])
     (g g 5))) => "120"]
)
