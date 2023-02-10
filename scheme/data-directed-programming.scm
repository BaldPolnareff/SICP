(define (install-rectangular-package)
;; internal procedures
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (mag z)
        (sqrt (+ (square (real-part z)) (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))

;; interface to the rest of the system

    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'mag       '(rectangular) mag)
    (put 'angle     '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular 
         (lambda (x y) (tag (make-from-real-imag x y)))) 
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)

(define (install-polar-package)
;; internal procedures
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z) (* (magnitude z) (cos (angle z))))
    (define (imag-part z) (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
                (cons (sqrt (+ (square x) (square y)))
                      (atan y x)))
;; interface to the rest of the system
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
         (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method for these types - APPLY-GENERIC: " (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (Mag       z) (apply-generic 'magnitude z))
(define (Angle     z) (apply-generic 'angle     z))

(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) x y))

; SICP Exercise 2.73

;(define (operator expr)
;    (car expr))
;
;(define (operands expr)
;    (cdr expr))
;
;(define (deriv expr var)
;    (cond ((number? expr) 0)
;          ((variable? expr) (if (same-variable? expr var) 1 0))
;          (else ((get 'deriv (operator expr))
;                 (operands expr) var))))
;
;(define (install-deriv-package)
; internal procedures
;    (define (variable? e)
;        (symbol? e))
;    (define (same-variable? v1 v2)
;        (and (variable? v1) (equal? v1 v2)))

;    (define (=number? expr num)
;        (and (number? expr) (= expr num)))
;
;    (define (make-sum a1 a2)
;        (cond ((and (number? a1) (number? a2)) (+ a1 a2))
;              ((=number? a1 0) a2)
;              ((=number? a2 0) a1)
;              (else (list '+ a1 a2))))
;    (define (make-product m1 m2)
;        (cond ((and (number? m1) (number? m2)) (* m1 m2))
;              ((or (=number? m1 0) (=number? m2 0)) 0)
;              ((=number? m1 1) m2)
;              ((=number? m2 1) m1)
;              (else (list '* m1 m2))))
;    (define (sum? x)
;        (and (pair? x) (eq? (car x) '+)))
;    (define (addend s)
;        (if (sum? s)
;            (cadr s)
;            (error "Not a sum")))
;    (define (augend s)
;        (if (sum? s)
;            (caddr s)
;            (error "Not a sum")))
;    (define (product? x)
;        (and (pair? x) (eq? (car x) '*)))
;    (define (multiplier p)
;        (if (product? p)
;            (cadr p)
;            (error "Not a product")))
;    (define (multiplicand p)
;        (if (product? p)
;            (caddr p)
;            (error "Not a product")))
;
;    (define (exponentiation? x)
;        (and (pair? x) (eq? (car x) '**)))
;    (define (base e)
;        (if (exponentiation? e)
;            (cadr e)
;            (error "Not an exponentiation")))
;    (define (exponent e)
;        (if (exponentiation? e)
;            (caddr e)
;            (error "Not an exponentiation")))
;
;    (define (make-exponentiation base exp)
;        (cond ((and (number? base) (number? exp)) (expt base exp))
;              ((=number? exp 0) 1)
;              ((=number? exp 1) base)
;              (else (list '** base exp))))
;
;
;; interface to the rest of the system
;
;    (define (tag x) (attach-tag 'deriv x))
;    (put 'addend '(deriv) addend)
;    (put 'augend '(deriv) augend)
;    (put 'multiplicand '(deriv) multiplicand)
;    (put 'multiplier '(deriv) multiplier)
;    (put 'base '(deriv) base)
;    (put 'exponent '(deriv) exponent)
;    (put 'make-sum 'deriv
;         (lambda (x y) (tag (make-sum x y))))
;    (put 'make-product 'deriv
;         (lambda (x y) (tag (make-product x y))))        
;    (put 'make-exponentiation 'deriv
;         (lambda (x y) (tag (make-exponentiation x y))))    
;
;    (define (apply-generic op . args)
;    (let ((type-tags (map type-tag args)))
;         (let ((proc (get op type-tags)))
;            (if proc
;                (apply proc (map contents args))
;                (error "No method for these types - APPLY-GENERIC: " (list op type-tags))))))
;
;    (define (addend s) (apply-generic 'addend s))
;    (define (augend a) (apply-generic 'augend s))
;    (define (multiplicand p) (apply-generic 'multiplicand p))
;    (define (multiplier p) (apply-generic 'multiplier p))
;    (define (base exp) (apply-generic 'base exp))
;    (define (exponent exp) (apply-generic 'exponent exp))
;
;
;    (define (make-sum x y)
;        ((get 'make-sum 'deriv) x y))
;    (define (make-product x y)
;        ((get 'make-product 'deriv) x y))
;    (define (make-exponentiation a b)
;        ((get 'make-exponentiation 'deriv) a b))
;'done))

(define (operator expr)
    (car expr))
(define (operands expr)
    (cdr expr))
(define (variable? e)
    (symbol? e))
(define (same-variable? v1 v2)
    (and (variable? v1) (equal? v1 v2)))
(define (=number? expr num)
    (and (number? expr) (= expr num)))
(define (deriv expr var)
    (cond ((number? expr) 0)
          ((variable? expr) (if (same-variable? expr var) 1 0))
          (else ((get 'deriv (operator expr))
                 (operands expr) var))))

(define (install-sum-package)
    (define (derivative expr var)
        (make-sum (deriv (car expr) var)
                  (deriv (cadr expr) var)))
    (put 'deriv '+ derivative))

(define (install-product-package)
    (define (derivative expr var)
        (make-sum
            (make-product (car expr)
                          (deriv (cadr expr) var))
            (make-product (deriv (car expr) var)
                          (cadr expr))))
    (put 'deriv '* derivative))

(define (install-exponentiation-package)
    (define (derivative expr var)
        (make-product (cadr expr) 
            (make-product (make-exponentiation (car expr) (make-sum (cadr expr) -1)) 
                          (deriv (car expr) var))))
    (put 'deriv '** derivative))