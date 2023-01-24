; SICP Exercise 1.46

(define (iterative-improve improvefun good-enough?)
    (define (iter guess)
        if (good-enough? guess)
        guess
        (iter (improvefun guess))
    )
    (lambda (guess) 
        (iter guess))
)

(define (derivative f dx)
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform f dx)
    (lambda (x) (- x (/ (f x) ((derivative f dx) x)))))

; define (newton-method fun guess dx tol)
;    (fixed-point (newton-transform fun dx) guess tol))

; (define (fixed-point fun x0 tol)
;     (define (close-enough? a b)
;         (< (abs (- b a)) tol))
;     (define (iter x)
;         (let ((next (fun x)))
;             (newline)
;             (display next)
;             (newline)
;             (
;                 if (close-enough? x next)
;                     next 
;                     (iter next)
;             )
;         )
;     )
;     (iter x0)
; )


(define (sqrt x tol)
    (define (avg a b)
    (/ (+ a b) 2))
    (define (square n)
        (* n n))
    (define (close-enough? guess)
        (< (abs (- (square guess) x)) tol))
    (define (improve guess)
        (avg guess (/ x guess)))
    ((iterative-improve improve close-enough?) 1.0)
)