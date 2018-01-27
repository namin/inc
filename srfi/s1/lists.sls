#!r6rs

;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;     -Olin

;; Ikarus porting begun by Abdulaziz Ghuloum,
;; and continued by Derick Eddington.

(library (srfi s1 lists)
  (export
   xcons make-list list-tabulate list-copy
   proper-list? circular-list? dotted-list? not-pair? null-list? list=
   circular-list length+
   iota
   first second third fourth fifth sixth seventh eighth ninth tenth
   car+cdr
   take       drop
   take-right drop-right
   take!      drop-right!
   split-at   split-at!
   last last-pair
   zip unzip1 unzip2 unzip3 unzip4 unzip5
   count
   append! append-reverse append-reverse! concatenate concatenate!
   unfold       fold       pair-fold       reduce
   unfold-right            pair-fold-right reduce-right
   append-map append-map! map! pair-for-each filter-map map-in-order
   filter! partition! remove!
   find-tail any every list-index
   take-while drop-while take-while!
   span break span! break!
   delete delete!
   alist-cons alist-copy
   delete-duplicates delete-duplicates!
   alist-delete alist-delete!
   reverse!
   lset<= lset= lset-adjoin
   lset-union  lset-intersection  lset-difference  lset-xor
   lset-diff+intersection
   lset-union! lset-intersection! lset-difference! lset-xor!
   lset-diff+intersection!
   ;; re-exported:
   append assq assv caaaar caaadr caaar caadar caaddr
   caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
   car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar
   cddadr cddar cdddar cddddr cdddr cddr cdr cons cons*
   length list list-ref memq memv null? pair?
   reverse set-car! set-cdr!
   ;; different than R6RS:
   assoc filter find fold-right for-each map member partition remove)
  (import
   (except (rnrs)
           assoc error filter find fold-right
           for-each map member partition remove)
   (rnrs mutable-pairs))

  (define-syntax check-arg
    (lambda (stx)
      (syntax-case stx ()
        [(_ pred val caller)
         (and (identifier? #'val) (identifier? #'caller))
         #'(unless (pred val)
             (assertion-violation 'caller "check-arg failed" val))])))

  (define (error . args)
    (if (and (<= 2 (length args)) (symbol? (car args)) (string? (cadr args)))
        (apply assertion-violation args)
        (apply assertion-violation "(library (srfi s1 lists))"
               "misuse of error procedure" args)))

  ;; Constructors
  ;; ;;;;;;;;;;;;;

  (define (xcons d a) (cons a d))

  (define (make-list len . maybe-elt)
    (check-arg (lambda (n) (and (integer? n) (>= n 0))) len make-list)
    (let ((elt (cond ((null? maybe-elt) #f) ; Default value
                     ((null? (cdr maybe-elt)) (car maybe-elt))
                     (else (error 'make-list "Too many arguments"
                                  (cons len maybe-elt))))))
      (do ((i len (- i 1))
           (ans '() (cons elt ans)))
          ((<= i 0) ans))))

  (define (list-tabulate len proc)
    (check-arg (lambda (n) (and (integer? n) (>= n 0))) len list-tabulate)
    (check-arg procedure? proc list-tabulate)
    (do ((i (- len 1) (- i 1))
         (ans '() (cons (proc i) ans)))
        ((< i 0) ans)))

  (define (list-copy lis)
    (let recur ((lis lis))
      (if (pair? lis)
          (cons (car lis) (recur (cdr lis)))
          lis)))

  (define iota
    (case-lambda
     [(count) (iota count 0 1)]
     [(count start) (iota count start 1)]
     [(count start step)
      (check-arg integer? count iota)
      (if (< count 0) (error 'iota "Negative step count" count))
      (check-arg number? start iota)
      (check-arg number? step iota)
      (let ((last-val (+ start (* (- count 1) step))))
        (do ((count count (- count 1))
             (val last-val (- val step))
             (ans '() (cons val ans)))
            ((<= count 0)  ans)))]))

  (define (circular-list val1 . vals)
    (let ((ans (cons val1 vals)))
      (set-cdr! (last-pair ans) ans)
      ans))

  (define (proper-list? x)
    (let lp ((x x) (lag x))
      (if (pair? x)
          (let ((x (cdr x)))
            (if (pair? x)
                (let ((x   (cdr x))
                      (lag (cdr lag)))
                  (and (not (eq? x lag)) (lp x lag)))
                (null? x)))
          (null? x))))

  (define (dotted-list? x)
    (let lp ((x x) (lag x))
      (if (pair? x)
          (let ((x (cdr x)))
            (if (pair? x)
                (let ((x   (cdr x))
                      (lag (cdr lag)))
                  (and (not (eq? x lag)) (lp x lag)))
                (not (null? x))))
          (not (null? x)))))

  (define (circular-list? x)
    (let lp ((x x) (lag x))
      (and (pair? x)
           (let ((x (cdr x)))
             (and (pair? x)
                  (let ((x   (cdr x))
                        (lag (cdr lag)))
                    (or (eq? x lag) (lp x lag))))))))

  (define (not-pair? x) (not (pair? x)))	; Inline me.

  (define (null-list? l)
    (cond ((pair? l) #f)
          ((null? l) #t)
          (else (error 'null-list? "argument out of domain" l))))


  (define (list= elt= . lists)
    (or (null? lists) ; special case
        (let lp1 ((list-a (car lists)) (others (cdr lists)))
          (or (null? others)
              (let ((list-b-orig (car others))
                    (others      (cdr others)))
                (if (eq? list-a list-b-orig)	; EQ? => LIST=
                    (lp1 list-b-orig others)
                    (let lp2 ((list-a list-a) (list-b list-b-orig))
                      (if (null-list? list-a)
                          (and (null-list? list-b)
                               (lp1 list-b-orig others))
                          (and (not (null-list? list-b))
                               (elt= (car list-a) (car list-b))
                               (lp2 (cdr list-a) (cdr list-b)))))))))))

  (define (length+ x)			; Returns #f if X is circular.
    (let lp ((x x) (lag x) (len 0))
      (if (pair? x)
          (let ((x (cdr x))
                (len (+ len 1)))
            (if (pair? x)
                (let ((x   (cdr x))
                      (lag (cdr lag))
                      (len (+ len 1)))
                  (and (not (eq? x lag)) (lp x lag len)))
                len))
          len)))

  (define (zip list1 . more-lists) (apply map list list1 more-lists))


  ;; Selectors
  ;; ;;;;;;;;;;

  (define first  car)
  (define second cadr)
  (define third  caddr)
  (define fourth cadddr)
  (define (fifth   x) (car    (cddddr x)))
  (define (sixth   x) (cadr   (cddddr x)))
  (define (seventh x) (caddr  (cddddr x)))
  (define (eighth  x) (cadddr (cddddr x)))
  (define (ninth   x) (car  (cddddr (cddddr x))))
  (define (tenth   x) (cadr (cddddr (cddddr x))))

  (define (car+cdr pair) (values (car pair) (cdr pair)))

  (define (take lis k)
    (check-arg integer? k take)
    (let recur ((lis lis) (k k))
      (if (zero? k) '()
          (cons (car lis)
                (recur (cdr lis) (- k 1))))))

  (define (drop lis k)
    (check-arg integer? k drop)
    (let iter ((lis lis) (k k))
      (if (zero? k) lis (iter (cdr lis) (- k 1)))))

  (define (take! lis k)
    (check-arg integer? k take!)
    (if (zero? k) '()
        (begin (set-cdr! (drop lis (- k 1)) '())
               lis)))

  (define (take-right lis k)
    (check-arg integer? k take-right)
    (let lp ((lag lis)  (lead (drop lis k)))
      (if (pair? lead)
          (lp (cdr lag) (cdr lead))
          lag)))

  (define (drop-right lis k)
    (check-arg integer? k drop-right)
    (let recur ((lag lis) (lead (drop lis k)))
      (if (pair? lead)
          (cons (car lag) (recur (cdr lag) (cdr lead)))
          '())))

  (define (drop-right! lis k)
    (check-arg integer? k drop-right!)
    (let ((lead (drop lis k)))
      (if (pair? lead)

          (let lp ((lag lis)  (lead (cdr lead)))	; Standard case
            (if (pair? lead)
                (lp (cdr lag) (cdr lead))
                (begin (set-cdr! lag '())
                       lis)))

          '())))	; Special case dropping everything -- no cons to side-effect.

  (define-syntax receive
    (syntax-rules ()
      [(_ (id* ...) expr body body* ...)
       (let-values ([(id* ...) expr]) body body* ...)]))


  (define (split-at x k)
    (check-arg integer? k split-at)
    (let recur ((lis x) (k k))
      (if (zero? k) (values '() lis)
          (receive (prefix suffix) (recur (cdr lis) (- k 1))
                   (values (cons (car lis) prefix) suffix)))))

  (define (split-at! x k)
    (check-arg integer? k split-at!)
    (if (zero? k) (values '() x)
        (let* ((prev (drop x (- k 1)))
               (suffix (cdr prev)))
          (set-cdr! prev '())
          (values x suffix))))


  (define (last lis) (car (last-pair lis)))

  (define (last-pair lis)
    (check-arg pair? lis last-pair)
    (let lp ((lis lis))
      (let ((tail (cdr lis)))
        (if (pair? tail) (lp tail) lis))))


  ;; Unzippers -- 1 through 5
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (unzip1 lis) (map car lis))

  (define (unzip2 lis)
    (let recur ((lis lis))
      (if (null-list? lis) (values lis lis)	; Use NOT-PAIR? to handle
          (let ((elt (car lis)))			; dotted lists.
            (receive (a b) (recur (cdr lis))
                     (values (cons (car  elt) a)
                             (cons (cadr elt) b)))))))

  (define (unzip3 lis)
    (let recur ((lis lis))
      (if (null-list? lis) (values lis lis lis)
          (let ((elt (car lis)))
            (receive (a b c) (recur (cdr lis))
                     (values (cons (car   elt) a)
                             (cons (cadr  elt) b)
                             (cons (caddr elt) c)))))))

  (define (unzip4 lis)
    (let recur ((lis lis))
      (if (null-list? lis) (values lis lis lis lis)
          (let ((elt (car lis)))
            (receive (a b c d) (recur (cdr lis))
                     (values (cons (car    elt) a)
                             (cons (cadr   elt) b)
                             (cons (caddr  elt) c)
                             (cons (cadddr elt) d)))))))

  (define (unzip5 lis)
    (let recur ((lis lis))
      (if (null-list? lis) (values lis lis lis lis lis)
          (let ((elt (car lis)))
            (receive (a b c d e) (recur (cdr lis))
                     (values (cons (car     elt) a)
                             (cons (cadr    elt) b)
                             (cons (caddr   elt) c)
                             (cons (cadddr  elt) d)
                             (cons (car (cddddr  elt)) e)))))))


  ;; append! append-reverse append-reverse! concatenate concatenate!
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (append! . lists)
    ;; First, scan through lists looking for a non-empty one.
    (let lp ((lists lists) (prev '()))
      (if (not (pair? lists)) prev
          (let ((first (car lists))
                (rest (cdr lists)))
            (if (not (pair? first)) (lp rest first)

                ;; Now, do the splicing.
                (let lp2 ((tail-cons (last-pair first))
                          (rest rest))
                  (if (pair? rest)
                      (let ((next (car rest))
                            (rest (cdr rest)))
                        (set-cdr! tail-cons next)
                        (lp2 (if (pair? next) (last-pair next) tail-cons)
                             rest))
                      first)))))))

  (define (append-reverse rev-head tail)
    (let lp ((rev-head rev-head) (tail tail))
      (if (null-list? rev-head) tail
          (lp (cdr rev-head) (cons (car rev-head) tail)))))

  (define (append-reverse! rev-head tail)
    (let lp ((rev-head rev-head) (tail tail))
      (if (null-list? rev-head) tail
          (let ((next-rev (cdr rev-head)))
            (set-cdr! rev-head tail)
            (lp next-rev rev-head)))))


  (define (concatenate  lists) (reduce-right append  '() lists))
  (define (concatenate! lists) (reduce-right append! '() lists))

  ;; Fold/map internal utilities
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (%cdrs lists)
    (call-with-current-continuation
     (lambda (abort)
       (let recur ((lists lists))
         (if (pair? lists)
             (let ((lis (car lists)))
               (if (null-list? lis) (abort '())
                   (cons (cdr lis) (recur (cdr lists)))))
             '())))))

  (define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
    (let recur ((lists lists))
      (if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))))

  (define (%cars+cdrs lists)
    (let f ([ls lists] [a* '()] [d* '()])
      (cond
       [(pair? ls)
        (let ([a (car ls)])
          (if (pair? a)
              (f (cdr ls) (cons (car a) a*) (cons (cdr a) d*))
              (values '() '())))]
       [else (values (reverse a*) (reverse d*))])))

  (define (%cars+cdrs+ lists cars-final)
    (call-with-current-continuation
     (lambda (abort)
       (let recur ((lists lists))
         (if (pair? lists)
             (receive (list other-lists) (car+cdr lists)
                      (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
                          (receive (a d) (car+cdr list)
                                   (receive (cars cdrs) (recur other-lists)
                                            (values (cons a cars) (cons d cdrs))))))
             (values (list cars-final) '()))))))

  (define (%cars+cdrs/no-test lists)
    (let recur ((lists lists))
      (if (pair? lists)
          (receive (list other-lists) (car+cdr lists)
                   (receive (a d) (car+cdr list)
                            (receive (cars cdrs) (recur other-lists)
                                     (values (cons a cars) (cons d cdrs)))))
          (values '() '()))))


  ;; count
  ;; ;;;;;;
  (define (count pred list1 . lists)
    (check-arg procedure? pred count)
    (if (pair? lists)

        ;; N-ary case
        (let lp ((list1 list1) (lists lists) (i 0))
          (if (null-list? list1) i
              (receive (as ds) (%cars+cdrs lists)
                       (if (null? as) i
                           (lp (cdr list1) ds
                               (if (apply pred (car list1) as) (+ i 1) i))))))

        ;; Fast path
        (let lp ((lis list1) (i 0))
          (if (null-list? lis) i
              (lp (cdr lis) (if (pred (car lis)) (+ i 1) i))))))


  ;; fold/unfold
  ;; ;;;;;;;;;;;;

  (define unfold-right
    (case-lambda
     [(p f g seed)
      (unfold-right p f g seed '())]
     [(p f g seed tail)
      (check-arg procedure? p unfold-right)
      (check-arg procedure? f unfold-right)
      (check-arg procedure? g unfold-right)
      (let lp ((seed seed) (ans tail))
        (if (p seed) ans
            (lp (g seed)
                (cons (f seed) ans))))]))


  (define (unfold p f g seed . maybe-tail-gen)
    (check-arg procedure? p unfold)
    (check-arg procedure? f unfold)
    (check-arg procedure? g unfold)
    (if (pair? maybe-tail-gen) ;;; so much for :optional (aghuloum)

        (let ((tail-gen (car maybe-tail-gen)))
          (if (pair? (cdr maybe-tail-gen))
              (apply error 'unfold "Too many arguments" p f g seed maybe-tail-gen)

              (let recur ((seed seed))
                (if (p seed) (tail-gen seed)
                    (cons (f seed) (recur (g seed)))))))

        (let recur ((seed seed))
          (if (p seed) '()
              (cons (f seed) (recur (g seed)))))))


  (define (fold kons knil lis1 . lists)
    (check-arg procedure? kons fold)
    (if (pair? lists)
        (let lp ((lists (cons lis1 lists)) (ans knil))	; N-ary case
          (receive (cars+ans cdrs) (%cars+cdrs+ lists ans)
                   (if (null? cars+ans) ans ; Done.
                       (lp cdrs (apply kons cars+ans)))))

        (let lp ((lis lis1) (ans knil))			; Fast path
          (if (null-list? lis) ans
              (lp (cdr lis) (kons (car lis) ans))))))


  (define (fold-right kons knil lis1 . lists)
    (check-arg procedure? kons fold-right)
    (if (pair? lists)
        (let recur ((lists (cons lis1 lists)))		; N-ary case
          (let ((cdrs (%cdrs lists)))
            (if (null? cdrs) knil
                (apply kons (%cars+ lists (recur cdrs))))))

        (let recur ((lis lis1))				; Fast path
          (if (null-list? lis) knil
              (let ((head (car lis)))
                (kons head (recur (cdr lis))))))))


  (define (pair-fold-right f zero lis1 . lists)
    (check-arg procedure? f pair-fold-right)
    (if (pair? lists)
        (let recur ((lists (cons lis1 lists)))		; N-ary case
          (let ((cdrs (%cdrs lists)))
            (if (null? cdrs) zero
                (apply f (append! lists (list (recur cdrs)))))))

        (let recur ((lis lis1))				; Fast path
          (if (null-list? lis) zero (f lis (recur (cdr lis)))))))

  (define (pair-fold f zero lis1 . lists)
    (check-arg procedure? f pair-fold)
    (if (pair? lists)
        (let lp ((lists (cons lis1 lists)) (ans zero))	; N-ary case
          (let ((tails (%cdrs lists)))
            (if (null? tails) ans
                (lp tails (apply f (append! lists (list ans)))))))

        (let lp ((lis lis1) (ans zero))
          (if (null-list? lis) ans
              (let ((tail (cdr lis)))		; Grab the cdr now,
                (lp tail (f lis ans)))))))	; in case F SET-CDR!s LIS.

  ;; REDUCE and REDUCE-RIGHT only use RIDENTITY in the empty-list case.
  ;; These cannot meaningfully be n-ary.

  (define (reduce f ridentity lis)
    (check-arg procedure? f reduce)
    (if (null-list? lis) ridentity
        (fold f (car lis) (cdr lis))))

  (define (reduce-right f ridentity lis)
    (check-arg procedure? f reduce-right)
    (if (null-list? lis) ridentity
        (let recur ((head (car lis)) (lis (cdr lis)))
          (if (pair? lis)
              (f head (recur (car lis) (cdr lis)))
              head))))

  ;; Mappers: append-map append-map! pair-for-each map! filter-map map-in-order
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (append-map f lis1 . lists)
    (check-arg procedure? f append-map)
    (really-append-map append  f lis1 lists))
  (define (append-map! f lis1 . lists)
    (check-arg procedure? f append-map!)
    (really-append-map append! f lis1 lists))

  (define (really-append-map appender f lis1 lists)
    (if (pair? lists)
        (receive (cars cdrs) (%cars+cdrs (cons lis1 lists))
                 (if (null? cars) '()
                     (let recur ((cars cars) (cdrs cdrs))
                       (let ((vals (apply f cars)))
                         (receive (cars2 cdrs2) (%cars+cdrs cdrs)
                                  (if (null? cars2) vals
                                      (appender vals (recur cars2 cdrs2))))))))

        ;; Fast path
        (if (null-list? lis1) '()
            (let recur ((elt (car lis1)) (rest (cdr lis1)))
              (let ((vals (f elt)))
                (if (null-list? rest) vals
                    (appender vals (recur (car rest) (cdr rest)))))))))


  (define (pair-for-each proc lis1 . lists)
    (check-arg procedure? proc pair-for-each)
    (if (pair? lists)

        (let lp ((lists (cons lis1 lists)))
          (let ((tails (%cdrs lists)))
            (if (pair? tails)
                (begin (apply proc lists)
                       (lp tails)))))

        ;; Fast path.
        (let lp ((lis lis1))
          (if (not (null-list? lis))
              (let ((tail (cdr lis)))	; Grab the cdr now,
                (proc lis)		; in case PROC SET-CDR!s LIS.
                (lp tail))))))

  ;; We stop when LIS1 runs out, not when any list runs out.
  (define (map! f lis1 . lists)
    (check-arg procedure? f map!)
    (if (pair? lists)
        (let lp ((lis1 lis1) (lists lists))
          (if (not (null-list? lis1))
              (receive (heads tails) (%cars+cdrs/no-test lists)
                       (set-car! lis1 (apply f (car lis1) heads))
                       (lp (cdr lis1) tails))))

        ;; Fast path.
        (pair-for-each (lambda (pair) (set-car! pair (f (car pair)))) lis1))
    lis1)


  ;; Map F across L, and save up all the non-false results.
  (define (filter-map f lis1 . lists)
    (check-arg procedure? f filter-map)
    (if (pair? lists)
        (let recur ((lists (cons lis1 lists)))
          (receive (cars cdrs) (%cars+cdrs lists)
                   (if (pair? cars)
                       (cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
                             (else (recur cdrs))) ; Tail call in this arm.
                       '())))

        ;; Fast path.
        (let recur ((lis lis1))
          (if (null-list? lis) lis
              (let ((tail (recur (cdr lis))))
                (cond ((f (car lis)) => (lambda (x) (cons x tail)))
                      (else tail)))))))


  ;; Map F across lists, guaranteeing to go left-to-right.
  ;; NOTE: Some implementations of R5RS MAP are compliant with this spec;
  ;; in which case this procedure may simply be defined as a synonym for MAP.

  (define (map-in-order f lis1 . lists)
    (check-arg procedure? f map-in-order)
    (if (pair? lists)
        (let recur ((lists (cons lis1 lists)))
          (receive (cars cdrs) (%cars+cdrs lists)
                   (if (pair? cars)
                       (let ((x (apply f cars)))		; Do head first,
                         (cons x (recur cdrs)))		; then tail.
                       '())))

        ;; Fast path.
        (let recur ((lis lis1))
          (if (null-list? lis) lis
              (let ((tail (cdr lis))
                    (x (f (car lis))))		; Do head first,
                (cons x (recur tail)))))))	; then tail.


  ;; We extend MAP to handle arguments of unequal length.
  (define map map-in-order)

  ;; Contributed by Michael Sperber since it was missing from the
  ;; reference implementation.
  (define (for-each f lis1 . lists)
    (if (pair? lists)
        (let recur ((lists (cons lis1 lists)))
          (receive (cars cdrs) (%cars+cdrs lists)
                   (if (pair? cars)
                       (begin
                         (apply f cars)	; Do head first,
                         (recur cdrs)))))	; then tail.

        ;; Fast path.
        (let recur ((lis lis1))
          (if (not (null-list? lis))
              (begin
                (f (car lis))		; Do head first,
                (recur (cdr lis)))))))	; then tail.

  ;; filter, remove, partition
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; FILTER, REMOVE, PARTITION and their destructive counterparts do not
  ;; disorder the elements of their argument.

  ;; This FILTER shares the longest tail of L that has no deleted elements.
  ;; If Scheme had multi-continuation calls, they could be made more efficient.

  (define (filter pred lis)			; Sleazing with EQ? makes this
    (check-arg procedure? pred filter)		; one faster.
    (let recur ((lis lis))
      (if (null-list? lis) lis			; Use NOT-PAIR? to handle dotted lists.
          (let ((head (car lis))
                (tail (cdr lis)))
            (if (pred head)
                (let ((new-tail (recur tail)))	; Replicate the RECUR call so
                  (if (eq? tail new-tail) lis
                      (cons head new-tail)))
                (recur tail))))))			; this one can be a tail call.


  (define (filter! pred lis)
    (check-arg procedure? pred filter!)
    (let lp ((ans lis))
      (cond ((null-list? ans)       ans)			; Scan looking for
            ((not (pred (car ans))) (lp (cdr ans)))	; first cons of result.
            (else (letrec ((scan-in (lambda (prev lis)
                                      (if (pair? lis)
                                          (if (pred (car lis))
                                              (scan-in lis (cdr lis))
                                              (scan-out prev (cdr lis))))))
                           (scan-out (lambda (prev lis)
                                       (let lp ((lis lis))
                                         (if (pair? lis)
                                             (if (pred (car lis))
                                                 (begin (set-cdr! prev lis)
                                                        (scan-in lis (cdr lis)))
                                                 (lp (cdr lis)))
                                             (set-cdr! prev lis))))))
                    (scan-in ans (cdr ans))
                    ans)))))

  (define (partition pred lis)
    (check-arg procedure? pred partition)
    (let recur ((lis lis))
      (if (null-list? lis) (values lis lis)	; Use NOT-PAIR? to handle dotted lists.
          (let ((elt (car lis))
                (tail (cdr lis)))
            (receive (in out) (recur tail)
                     (if (pred elt)
                         (values (if (pair? out) (cons elt in) lis) out)
                         (values in (if (pair? in) (cons elt out) lis))))))))

  (define (partition! pred lis)
    (check-arg procedure? pred partition!)
    (if (null-list? lis) (values lis lis)
        (letrec ((scan-in (lambda (in-prev out-prev lis)
                            (let lp ((in-prev in-prev) (lis lis))
                              (if (pair? lis)
                                  (if (pred (car lis))
                                      (lp lis (cdr lis))
                                      (begin (set-cdr! out-prev lis)
                                             (scan-out in-prev lis (cdr lis))))
                                  (set-cdr! out-prev lis))))) ; Done.

                 (scan-out (lambda (in-prev out-prev lis)
                             (let lp ((out-prev out-prev) (lis lis))
                               (if (pair? lis)
                                   (if (pred (car lis))
                                       (begin (set-cdr! in-prev lis)
                                              (scan-in lis out-prev (cdr lis)))
                                       (lp lis (cdr lis)))
                                   (set-cdr! in-prev lis)))))) ; Done.

          ;; Crank up the scan&splice loops.
          (if (pred (car lis))
              ;; LIS begins in-list. Search for out-list's first pair.
              (let lp ((prev-l lis) (l (cdr lis)))
                (cond ((not (pair? l)) (values lis l))
                      ((pred (car l)) (lp l (cdr l)))
                      (else (scan-out prev-l l (cdr l))
                            (values lis l))))	; Done.

              ;; LIS begins out-list. Search for in-list's first pair.
              (let lp ((prev-l lis) (l (cdr lis)))
                (cond ((not (pair? l)) (values l lis))
                      ((pred (car l))
                       (scan-in l prev-l (cdr l))
                       (values l lis))		; Done.
                      (else (lp l (cdr l)))))))))


  ;; Inline us, please.
  (define (remove  pred l) (filter  (lambda (x) (not (pred x))) l))
  (define (remove! pred l) (filter! (lambda (x) (not (pred x))) l))

  (define delete
    (case-lambda
     [(x lis)
      (delete x lis equal?)]
     [(x lis =)
      (filter (lambda (y) (not (= x y))) lis)]))

  (define delete!
    (case-lambda
     [(x lis)
      (delete! x lis equal?)]
     [(x lis =)
      (filter! (lambda (y) (not (= x y))) lis)]))

  ;; Extended from R4RS to take an optional comparison argument.
  (define member
    (case-lambda
     [(x lis)
      (member x lis equal?)]
     [(x lis =)
      (find-tail (lambda (y) (= x y)) lis)]))

  (define delete-duplicates
    (case-lambda
     [(lis)
      (delete-duplicates lis equal?)]
     [(lis elt=)
      (check-arg procedure? elt= delete-duplicates)
      (let recur ((lis lis))
        (if (null-list? lis) lis
            (let* ((x (car lis))
                   (tail (cdr lis))
                   (new-tail (recur (delete x tail elt=))))
              (if (eq? tail new-tail) lis (cons x new-tail)))))]))

  (define delete-duplicates!
    (case-lambda
     [(lis)
      (delete-duplicates! lis equal?)]
     [(lis elt=)
      (check-arg procedure? elt= delete-duplicates!)
      (let recur ((lis lis))
        (if (null-list? lis) lis
            (let* ((x (car lis))
                   (tail (cdr lis))
                   (new-tail (recur (delete! x tail elt=))))
              (when (not (eq? tail new-tail))
                (set-cdr! lis new-tail))
              lis)))]))


  ;; alist stuff
  ;; ;;;;;;;;;;;;

  (define assoc
    (case-lambda
     [(x lis)
      (assoc x lis equal?)]
     [(x lis =)
      (find (lambda (entry) (= x (car entry))) lis)]))

  (define (alist-cons key datum alist) (cons (cons key datum) alist))

  (define (alist-copy alist)
    (map (lambda (elt) (cons (car elt) (cdr elt)))
         alist))

  (define alist-delete
    (case-lambda
     [(key alist)
      (alist-delete key alist equal?)]
     [(key alist =)
      (filter (lambda (elt) (not (= key (car elt)))) alist)]))

  (define alist-delete!
    (case-lambda
     [(key alist)
      (alist-delete! key alist equal?)]
     [(key alist =)
      (filter! (lambda (elt) (not (= key (car elt)))) alist)]))


  ;; find find-tail take-while drop-while span break any every list-index
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (find pred list)
    (cond ((find-tail pred list) => car)
          (else #f)))

  (define (find-tail pred list)
    (check-arg procedure? pred find-tail)
    (let lp ((list list))
      (and (not (null-list? list))
           (if (pred (car list)) list
               (lp (cdr list))))))

  (define (take-while pred lis)
    (check-arg procedure? pred take-while)
    (let recur ((lis lis))
      (if (null-list? lis) '()
          (let ((x (car lis)))
            (if (pred x)
                (cons x (recur (cdr lis)))
                '())))))

  (define (drop-while pred lis)
    (check-arg procedure? pred drop-while)
    (let lp ((lis lis))
      (if (null-list? lis) '()
          (if (pred (car lis))
              (lp (cdr lis))
              lis))))

  (define (take-while! pred lis)
    (check-arg procedure? pred take-while!)
    (if (or (null-list? lis) (not (pred (car lis)))) '()
        (begin (let lp ((prev lis) (rest (cdr lis)))
                 (if (pair? rest)
                     (let ((x (car rest)))
                       (if (pred x) (lp rest (cdr rest))
                           (set-cdr! prev '())))))
               lis)))

  (define (span pred lis)
    (check-arg procedure? pred span)
    (let recur ((lis lis))
      (if (null-list? lis) (values '() '())
          (let ((x (car lis)))
            (if (pred x)
                (receive (prefix suffix) (recur (cdr lis))
                         (values (cons x prefix) suffix))
                (values '() lis))))))

  (define (span! pred lis)
    (check-arg procedure? pred span!)
    (if (or (null-list? lis) (not (pred (car lis)))) (values '() lis)
        (let ((suffix (let lp ((prev lis) (rest (cdr lis)))
                        (if (null-list? rest) rest
                            (let ((x (car rest)))
                              (if (pred x) (lp rest (cdr rest))
                                  (begin (set-cdr! prev '())
                                         rest)))))))
          (values lis suffix))))


  (define (break  pred lis) (span  (lambda (x) (not (pred x))) lis))
  (define (break! pred lis) (span! (lambda (x) (not (pred x))) lis))

  (define (any pred lis1 . lists)
    (check-arg procedure? pred any)
    (if (pair? lists)

        ;; N-ary case
        (receive (heads tails) (%cars+cdrs (cons lis1 lists))
                 (and (pair? heads)
                      (let lp ((heads heads) (tails tails))
                        (receive (next-heads next-tails) (%cars+cdrs tails)
                                 (if (pair? next-heads)
                                     (or (apply pred heads) (lp next-heads next-tails))
                                     (apply pred heads)))))) ; Last PRED app is tail call.

        ;; Fast path
        (and (not (null-list? lis1))
             (let lp ((head (car lis1)) (tail (cdr lis1)))
               (if (null-list? tail)
                   (pred head)		; Last PRED app is tail call.
                   (or (pred head) (lp (car tail) (cdr tail))))))))

  (define every
    (case-lambda
     [(p ls)
      (or (null-list? ls)
          (let f ([p p] [a (car ls)] [d (cdr ls)])
            (cond
             [(pair? d)
              (and (p a) (f p (car d) (cdr d)))]
             [else (p a)])))]
     [(p ls1 ls2)
      (cond
       [(and (pair? ls1) (pair? ls2))
        (let f ([p p] [a1 (car ls1)] [d1 (cdr ls1)] [a2 (car ls2)] [d2 (cdr ls2)])
          (cond
           [(and (pair? d1) (pair? d2))
            (and (p a1 a2) (f p (car d1) (cdr d1) (car d2) (cdr d2)))]
           [else (p a1 a2)]))]
       [else #t])]
     [(pred lis1 . lists)
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
               (or (not (pair? heads))
                   (let lp ((heads heads) (tails tails))
                     (receive (next-heads next-tails) (%cars+cdrs tails)
                              (if (pair? next-heads)
                                  (and (apply pred heads) (lp next-heads next-tails))
                                  (apply pred heads))))))]))

  (define (list-index pred lis1 . lists)
    (check-arg procedure? pred list-index)
    (if (pair? lists)

        ;; N-ary case
        (let lp ((lists (cons lis1 lists)) (n 0))
          (receive (heads tails) (%cars+cdrs lists)
                   (and (pair? heads)
                        (if (apply pred heads) n
                            (lp tails (+ n 1))))))

        ;; Fast path
        (let lp ((lis lis1) (n 0))
          (and (not (null-list? lis))
               (if (pred (car lis)) n (lp (cdr lis) (+ n 1)))))))

  ;; Reverse
  ;; ;;;;;;;;

  (define (reverse! lis)
    (let lp ((lis lis) (ans '()))
      (if (null-list? lis) ans
          (let ((tail (cdr lis)))
            (set-cdr! lis ans)
            (lp tail lis)))))

  ;; Lists-as-sets
  ;; ;;;;;;;;;;;;;;

  (define (%lset2<= = lis1 lis2) (every (lambda (x) (member x lis2 =)) lis1))

  (define (lset<= = . lists)
    (check-arg procedure? = lset<=)
    (or (not (pair? lists)) ; 0-ary case
        (let lp ((s1 (car lists)) (rest (cdr lists)))
          (or (not (pair? rest))
              (let ((s2 (car rest))  (rest (cdr rest)))
                (and (or (eq? s2 s1)	; Fast path
                         (%lset2<= = s1 s2)) ; Real test
                     (lp s2 rest)))))))

  (define (lset= = . lists)
    (check-arg procedure? = lset=)
    (or (not (pair? lists)) ; 0-ary case
        (let lp ((s1 (car lists)) (rest (cdr lists)))
          (or (not (pair? rest))
              (let ((s2   (car rest))
                    (rest (cdr rest)))
                (and (or (eq? s1 s2)	; Fast path
                         (and (%lset2<= = s1 s2) (%lset2<= = s2 s1))) ; Real test
                     (lp s2 rest)))))))


  (define (lset-adjoin = lis . elts)
    (check-arg procedure? = lset-adjoin)
    (fold (lambda (elt ans) (if (member elt ans =) ans (cons elt ans)))
          lis elts))


  (define (lset-union = . lists)
    (check-arg procedure? = lset-union)
    (reduce (lambda (lis ans)		; Compute ANS + LIS.
              (cond ((null? lis) ans)	; Don't copy any lists
                    ((null? ans) lis) 	; if we don't have to.
                    ((eq? lis ans) ans)
                    (else
                     (fold (lambda (elt ans) (if (any (lambda (x) (= x elt)) ans)
                                                 ans
                                                 (cons elt ans)))
                           ans lis))))
            '() lists))

  (define (lset-union! = . lists)
    (check-arg procedure? = lset-union!)
    (reduce (lambda (lis ans)		; Splice new elts of LIS onto the front of ANS.
              (cond ((null? lis) ans)	; Don't copy any lists
                    ((null? ans) lis) 	; if we don't have to.
                    ((eq? lis ans) ans)
                    (else
                     (pair-fold (lambda (pair ans)
                                  (let ((elt (car pair)))
                                    (if (any (lambda (x) (= x elt)) ans)
                                        ans
                                        (begin (set-cdr! pair ans) pair))))
                                ans lis))))
            '() lists))


  (define (lset-intersection = lis1 . lists)
    (check-arg procedure? = lset-intersection)
    (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
      (cond ((any null-list? lists) '())		; Short cut
            ((null? lists)          lis1)		; Short cut
            (else (filter (lambda (x)
                            (every (lambda (lis) (member x lis =)) lists))
                          lis1)))))

  (define (lset-intersection! = lis1 . lists)
    (check-arg procedure? = lset-intersection!)
    (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
      (cond ((any null-list? lists) '())		; Short cut
            ((null? lists)          lis1)		; Short cut
            (else (filter! (lambda (x)
                             (every (lambda (lis) (member x lis =)) lists))
                           lis1)))))


  (define (lset-difference = lis1 . lists)
    (check-arg procedure? = lset-difference)
    (let ((lists (filter pair? lists)))	; Throw out empty lists.
      (cond ((null? lists)     lis1)	; Short cut
            ((memq lis1 lists) '())	; Short cut
            (else (filter (lambda (x)
                            (every (lambda (lis) (not (member x lis =)))
                                   lists))
                          lis1)))))

  (define (lset-difference! = lis1 . lists)
    (check-arg procedure? = lset-difference!)
    (let ((lists (filter pair? lists)))	; Throw out empty lists.
      (cond ((null? lists)     lis1)	; Short cut
            ((memq lis1 lists) '())	; Short cut
            (else (filter! (lambda (x)
                             (every (lambda (lis) (not (member x lis =)))
                                    lists))
                           lis1)))))


  (define (lset-xor = . lists)
    (check-arg procedure? = lset-xor)
    (reduce (lambda (b a)			; Compute A xor B:
              ;; Note that this code relies on the constant-time
              ;; short-cuts provided by LSET-DIFF+INTERSECTION,
              ;; LSET-DIFFERENCE & APPEND to provide constant-time short
              ;; cuts for the cases A = (), B = (), and A eq? B. It takes
              ;; a careful case analysis to see it, but it's carefully
              ;; built in.

              ;; Compute a-b and a^b, then compute b-(a^b) and
              ;; cons it onto the front of a-b.
              (receive (a-b a-int-b)   (lset-diff+intersection = a b)
                       (cond ((null? a-b)     (lset-difference = b a))
                             ((null? a-int-b) (append b a))
                             (else (fold (lambda (xb ans)
                                           (if (member xb a-int-b =) ans (cons xb ans)))
                                         a-b
                                         b)))))
            '() lists))


  (define (lset-xor! = . lists)
    (check-arg procedure? = lset-xor!)
    (reduce (lambda (b a)			; Compute A xor B:
              ;; Note that this code relies on the constant-time
              ;; short-cuts provided by LSET-DIFF+INTERSECTION,
              ;; LSET-DIFFERENCE & APPEND to provide constant-time short
              ;; cuts for the cases A = (), B = (), and A eq? B. It takes
              ;; a careful case analysis to see it, but it's carefully
              ;; built in.

              ;; Compute a-b and a^b, then compute b-(a^b) and
              ;; cons it onto the front of a-b.
              (receive (a-b a-int-b)   (lset-diff+intersection! = a b)
                       (cond ((null? a-b)     (lset-difference! = b a))
                             ((null? a-int-b) (append! b a))
                             (else (pair-fold (lambda (b-pair ans)
                                                (if (member (car b-pair) a-int-b =) ans
                                                    (begin (set-cdr! b-pair ans) b-pair)))
                                              a-b
                                              b)))))
            '() lists))


  (define (lset-diff+intersection = lis1 . lists)
    (check-arg procedure? = lset-diff+intersection)
    (cond ((every null-list? lists) (values lis1 '()))	; Short cut
          ((memq lis1 lists)        (values '() lis1))	; Short cut
          (else (partition (lambda (elt)
                             (not (any (lambda (lis) (member elt lis =))
                                       lists)))
                           lis1))))

  (define (lset-diff+intersection! = lis1 . lists)
    (check-arg procedure? = lset-diff+intersection!)
    (cond ((every null-list? lists) (values lis1 '()))	; Short cut
          ((memq lis1 lists)        (values '() lis1))	; Short cut
          (else (partition! (lambda (elt)
                              (not (any (lambda (lis) (member elt lis =))
                                        lists)))
                            lis1))))
  ;; end of library
  )
