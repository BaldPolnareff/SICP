; SICP Exercise 1.31 
; Use the iterative product procedure to calculate pi using Wallis' formula

(define (iproduct f a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (f a) result)))    
    )
    (iter a 1)
)
(define (inc i)
    (+ i 1))

(define (wallis-pi n)
    (define (wallis k)
        (* (/ (* 2 k) (- (* 2 k) 1))
           (/ (* 2 k) (+ (* 2 k) 1)))
    )
    (* 2.0 (iproduct wallis 1 inc n))
)