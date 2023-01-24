; SICP Exercise 1.31

(define (sum Function a Next b)
    (if (> a b)
        0
        (+ (Function a)
           (sum Function (Next a) Next b)
        ))
)

(define (recursive-product function a next b)
    (if (> a b)
        1
        (* (function a)
           (recursive-product function (next a) next b)
        ))
)

(define (iterative-product function a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (function a) result)))
    )
    (iter a 1)
)

; Now let's reimplement the factorial function using product
; old implementation for reference: 

            ;;Implementing a counter function as a helper
            ;
            ;(define (ifactorial_helper product counter n)
            ;    (if (> counter n)
            ;        product
            ;        (ifactorial_helper (* product counter) (+ counter 1) n)))
            ;
            ;;Now implementing the function that iterates through the helper
            ;
            ;(define (ifactorial x)
            ;    (ifactorial_helper 1 1 x))

(define (identity x) x)

(define (inc n) (+ n 1))

(define (ifactorial n)
    (iterative-product identity 1 inc n)
)