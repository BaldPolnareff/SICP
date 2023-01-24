;SICP Exercise 1.15 

(define (exp_helper product base exponent)
    (if (= exponent 0)
        product
        (exp_helper (* base product) base (- exponent 1) )))

(define (iexp x y)
    (exp_helper 1 x y))

(define (f x)
    (- (* 3 x) (* 4 (iexp x 3)))
)

(define (sin x)
    (if (< (abs x) 0.1)
        x
        (f (sin (/ x 3)))    
    )
)
