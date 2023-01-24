; f(x) = x

(define (average a b)
    (/ (+ a b) 2))
(define (rangelen a b)
    (abs (- b a)))


(define (fixed-point fun x0 tol)
    (define (close-enough? a b)
        (< (abs (- b a)) tol))
    (define (iter x)
        (let ((next (fun x)))
            (
                if (close-enough? x next)
                    next 
                    (iter next)
            )
        )
    )
    (iter x0)
)

(define (sqrt x)
    (define (f y)
        (average y (/ x y)))
    (fixed-point f 1.0 0.00001)
)

; SICP Exercise 1.36

; Modify fixed-point so that it prints the
; sequence of approximations it generates, using the newline
; and display primitives shown in Exercise 1.22. en ﬁnd
; a solution to x^x = 1000 by ﬁnding a ﬁxed point of x→log(1000)/ log(x). 
; (Use Scheme’s primitive log procedure,
; which computes natural logarithms.) Compare the number
; of steps this takes with and without average damping. (Note
; that you cannot start fixed-point with a guess of 1, as this
; would cause division by log(1) = 0.)

(define (goldenratio x tol)
    (define (f x)
        (+ 1 (/ 1 x)))
    (fixed-point f x tol)
)

(define (fixed-point fun x0 tol)
    (define (close-enough? a b)
        (< (abs (- b a)) tol))
    (define (iter x)
        (let ((next (fun x)))
            (newline)
            (display next)
            (newline)
            (
                if (close-enough? x next)
                    next 
                    (iter next)
            )
        )
    )
    (iter x0)
)

(define (selfexp x)
    (fast-exp x x))

(define (selfexp_fp guess target tol)
    (define (f x)
        (/ (log target) (log x)))
    (fixed-point f guess tol)
)

; average dump x -> ln(1000)/ln(x) ===> 2x - x -> ln(1000)/ln(x); 2x -> ln(1000)/ln(x) + x; x -> 1/2 * (ln(1000)/ln(x) + x)

(define (selfexp_fp-avgdamp guess target tol)
    (define (f x)
        (* 0.5 (+ x (/ (log target) (log x)))))
    (fixed-point f guess tol)
)