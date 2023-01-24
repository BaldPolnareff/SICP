; SICP faster exponentiation 
; The idea is computing the exponentiation as a squaring operation for even exponents b^n = (b^(n/2)^2)
; and as the iterative iexp function for odd exponents b^n = b * b^(n-1)

(define (square x)
    (* x x))

; b^n = b * b^(n-1):

(define (exp_helper product base exponent)
    (if (= exponent 0)
        product
        (exp_helper (* base product) base (- exponent 1) )))

(define (iexp x y)
    (exp_helper 1 x y))

; Now let's define the complete exponentiation function by using a custom predicate expression

(define (even? n)
    (if (= n 0)
        #f
        (= (remainder n 2) 0)) 
)

(define (fast-exp x y)
    (if (even? y)
        (square (fast-exp x (/ y 2)))
        (iexp x y))
)