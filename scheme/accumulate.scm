; SICP Exercise 1.32
;Show that sum and product (Exercise 1.31) are both
;special cases of a still more general notion called accumulate
;that combines a collection of terms, using some gen-
;eral accumulation function:
;(accumulate combiner null-value term a next b)

;(define (iterative-product function a next b)
;    (define (iter a result)
;        (if (> a b)
;            result
;            (iter (next a) (* (function a) result)))
;    )
;    (iter a 1)
;)


(define (iaccumulate combiner nullv f a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (f a) result)))    
    )
    (iter a nullv)
)

(define (isum f a next b)
    (accumulate + 0 f a next b))

(define (iprod f a next b)
    (accumulate * 1 f a next b))

; let's now write a recursive accumulate procedure

(define (r-accumulate combiner nullv f a next b)
    (if (> a b)
        nullv
        (combiner (f a) (r-accumulate combiner nullv f (next a) next b))
    )
)



















