(define (GCD a b)
    (if (= b 0)
        a
        (GCD b (remainder a b))))

(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))

(define (numer x)
    (car x))
(define (denom x)
    (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define (add-rat a b)
    (make-rat (+ (* (numer a) (denom b)) (* (numer b) (denom a))) (* (denom a) (denom b)))
)

(define (sub-rat a b)
    (make-rat (- (* (numer a) (denom b)) (* (numer b) (denom a))) (* (denom a) (denom b)))
)

(define (mul-rat a b)
    (make-rat (* (numer a) (numer b)) (* (denom a) (denom b))))

(define (div-rad a b)
    (make-rat (* (numer a) (denom b)) (* (denom a) (numer b))))

(define (equal-rat? a b)
    (= (* (numer a) (denom b)) (* (numer b) (denom a))))

; SICP Exercise 2.1
; Deﬁne a better version of make-rat that han-
; dles both positive and negative arguments. make-rat should
; normalize the sign so that if the rational number is positive,
; both the numerator and denominator are positive, and if
; the rational number is negative, only the numerator is neg-
; ative.

; (define (make-rat n d)
;     (let ((g (gcd n d)))
;         (cons (/ n g) (/ d g))))

(define (make-rat n d)
    (let ((s (* (sgn n) (sgn d))) (g (gcd n d)))
        (
            if (> s 0)
                (cons (/ (abs n) g) (/ (abs d) g))
                (cons (/ (* (abs n) -1) g) (/ (abs d) g))
        ))
)



