(define-lib-primitive (length lst)
  (if (null? lst)
      0
      (fxadd1 (length (cdr lst)))))

(define-lib-primitive (fill args setop)
  (letrec ([rec (lambda (index args)
                  (unless (null? args)
                    (setop index (car args))
                    (rec (fxadd1 index) (cdr args))))])
    (rec 0 args)))
             
(define-lib-primitive (vector . args)
  (let ([v (make-vector (length args))])
    (fill args (lambda (index arg) (vector-set! v index arg)))
    v))

(define-lib-primitive (string . args)
  (let ([s (make-string (length args))])
    (fill args (lambda (index arg) (string-set! s index arg)))
    s))

(define-lib-primitive (list . args)
  args)

(define-lib-primitive (make_cell value)
  (cons value '()))

(define-lib-primitive (cell_get cell)
  (car cell))

(define-lib-primitive (cell_set cell value)
  (set-car! cell value))

(define-lib-primitive __symbols__
  (make_cell '()))

(define-lib-primitive (string=? s1 s2)
  (letrec ([rec (lambda (index)
                  (or (fx= index (string-length s1))
                      (and (char= (string-ref s1 index) (string-ref s2 index))
                           (rec (fxadd1 index)))))])
    (and (string? s1) (string? s2) (fx= (string-length s1) (string-length s2)) (rec 0))))
        
(define-lib-primitive (__find_symbol__ str)
  (letrec ([rec (lambda (symbols)
                  (cond
                   [(null? symbols) #f]
                   [(string=? str (string-symbol (car symbols))) (car symbols)]
                   [else (rec (cdr symbols))]))])
    (rec (cell_get __symbols__))))

(define-lib-primitive (string->symbol str)
  (or (__find_symbol__ str)
      (let ([symbol (make-symbol str)])
        (cell_set __symbols__ (cons symbol (cell_get __symbols__)))
        symbol)))

(define-lib-primitive (error . args)
  (foreign-call "ik_error" args))

(define-lib-primitive (log msg)
  (foreign-call "ik_log" msg))

(define-lib-primitive (string-set! s i c)
  (cond
   [(not (string? s)) (error)]
   [(not (fixnum? i)) (error)]
   [(not (char? c)) (error)]
   [(not (and (fx<= 0 i) (fx< i (string-length s)))) (error)]
   [else ($string-set! s i c)]))

(define-lib-primitive (string-ref s i)
  (cond
   [(not (string? s)) (error)]
   [(not (fixnum? i)) (error)]
   [(not (and (fx<= 0 i) (fx< i (string-length s)))) (error)]
   [else ($string-ref s i)]))

(define-lib-primitive (vector-set! v i e)
  (cond
   [(not (vector? v)) (error)]
   [(not (fixnum? i)) (error)]
   [(not (and (fx<= 0 i) (fx< i (vector-length v)))) (error)]
   [else ($vector-set! v i e)]))

(define-lib-primitive (vector-ref v i)
  (cond
   [(not (vector? v)) (error)]
   [(not (fixnum? i)) (error)]
   [(not (and (fx<= 0 i) (fx< i (vector-length v)))) (error)]
   [else ($vector-ref v i)]))

(define-lib-primitive (liftneg f a b)
  (cond
   [(and (fx< a 0) (fx>= b 0))
    (fx- 0 (f (fx- 0 a) b))]
   [(and (fx>= a 0) (fx< b 0))
    (fx- 0 (f a (fx- 0 b)))]
   [(and (fx< a 0) (fx< b 0))
    (f (fx- 0 a) (fx- 0 b))]
   [else
    (f a b)]))

(define-lib-primitive (liftneg1 f a b)
  (cond
   [(and (fx< a 0) (fx>= b 0))
    (fx- 0 (f (fx- 0 a) b))]
   [(and (fx>= a 0) (fx< b 0))
    (f a (fx- 0 b))]
   [(and (fx< a 0) (fx< b 0))
    (fx- 0 (f (fx- 0 a) (fx- 0 b)))]
   [else
    (f a b)]))
  
(define-lib-primitive (fxquotient a b)
  (liftneg (lambda (a b) ($fxquotient a b)) a b))

(define-lib-primitive (fxremainder a b)
  (liftneg1 (lambda (a b) ($fxremainder a b)) a b))

(define-lib-primitive (exit . args)
  (let ([status (if (null? args) 0 (car args))])
    (foreign-call "exit" status)))

(define-lib-primitive (s_write fd str len)
  (foreign-call "s_write" fd str len))

(define-lib-primitive stdout
  (make-output-port "" 1))

(define-lib-primitive (current-output-port)
  stdout)

(define-lib-primitive BUFFER_SIZE 4096)

(define-lib-primitive (open-output-file fname . args)
  (let ([fd (foreign-call "s_open_write" fname)])
    (make-output-port fname fd)))
    
(define-lib-primitive (make-output-port fname fd)
  (vector 'output-port fname fd (make-string BUFFER_SIZE) 0 BUFFER_SIZE))

(define-lib-primitive (output-port-fname port)
  (vector-ref port 1))

(define-lib-primitive (output-port-fd port)
  (vector-ref port 2))

(define-lib-primitive (output-port-buffer port)
  (vector-ref port 3))

(define-lib-primitive (output-port-buffer-index port)
  (vector-ref port 4))

(define-lib-primitive (output-port-buffer-size port)
  (vector-ref port 5))

(define-lib-primitive (set-output-port-buffer-index! port index)
  (vector-set! port 4 index))

(define-lib-primitive (inc-output-port-buffer-index! port)
  (set-output-port-buffer-index! port (fxadd1 (output-port-buffer-index port))))

(define-lib-primitive (write-char c (port (current-output-port)))
  (string-set! (output-port-buffer port) (output-port-buffer-index port) c)
  (inc-output-port-buffer-index! port)
  (when (fx= (output-port-buffer-index port) (output-port-buffer-size port))
	(output-port-write-buffer port)))

(define-lib-primitive (output-port? x)
  (and (vector? x) (fx= (vector-length x) 6) (eq? 'output-port (vector-ref x 0))))

(define-lib-primitive (output-port-write-buffer port)
  (s_write (output-port-fd port)
	   (output-port-buffer port)
	   (output-port-buffer-index port))
  (set-output-port-buffer-index! port 0))

(define-lib-primitive (flush-output-port (port (current-output-port)))
  (output-port-write-buffer port)
  (foreign-call "s_fflush" (output-port-fd port)))

(define-lib-primitive (close-output-port port)
  (flush-output-port port)
  (unless (string=? "" (output-port-fname port))
	  (foreign-call "s_close" (output-port-fd port))))

(define-lib-primitive (write x (port (current-output-port)))
  (flush-output-port port)
  ;; This is cheating... should write it in Scheme.
  (foreign-call "scheme_write" (output-port-fd port) x 0)
  (flush-output-port port))

(define-lib-primitive (display x (port (current-output-port)))
  (flush-output-port port)
  (foreign-call "scheme_write" (output-port-fd port) x 2)
  (flush-output-port port))

(define-lib-primitive (open-input-file fname . args)
  (let ([fd (foreign-call "s_open_read" fname)])
    (make-input-port fname fd)))
    
(define-lib-primitive (make-input-port fname fd)
  (vector 'input-port fname fd))

(define-lib-primitive (input-port-fname port)
  (vector-ref port 1))

(define-lib-primitive (input-port-fd port)
  (vector-ref port 2))

(define-lib-primitive (input-port? x)
  (and (vector? x) (fx= (vector-length x) 3) (eq? 'input-port (vector-ref x 0))))

(define-lib-primitive (read-char port)
  (foreign-call "s_read_char" (input-port-fd port)))

(define-lib-primitive (close-input-port port)
  (foreign-call "s_close" (input-port-fd port)))
