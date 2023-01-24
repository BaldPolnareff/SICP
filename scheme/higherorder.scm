; Sum of integers from a trough b

(define (sum-int a b)
    (if (> a b)
        0
        (+ a (sum-int (+ a 1) b)))
);

; Sum of the cubes of the integers from a through b

(define (cube x)
    (* x x x));

(define (sum-cube a b)
    (if (> a b)
        0
        (+ (cube a) (sum-cube (+ a 1) b)))
);

; Sum of the nth power of the integers from a through b, using the previously implemented fast-exp algorithm

(define (sum-nth a b N)
    (if (> a b)
        0
        (+ (fast-exp a N) (sum-nth (+ a 1) b N)))
);

; Leibniz's sum of a sequence that converges to π/8 = 1/(1*3) + 1/(5*7) + 1/(9*11) + ...

(define (pi-sum a b)
    (if (> a b)
        0
        (+ (/ 1 (* a (+ a 2))) 
           (pi-sum (+ a 4) b)))
);

; This approximates pi using this series!
(define (pi_leibniz a b)
    (* 8 (pi-sum a b))
)

; What if I wanted to write a higher-order procedure that generalizes the summation with this template?

;(define (<procedure> a b)
;    (if (> a b)
;        0
;        (+ <term> a) (<procedure> (<next-term> a) b))
;)

(define (sum Function a Next b)
    (if (> a b)
        0
        (+ (Function a)
           (sum Function (Next a) Next b)
        ))
)

; Now let's reimplement the cube summation using this generalized template

; This procedure is the custom next term
(define (inc n)
    (+ n 1)
)

(define (cube_sum a b)
    (sum cube a inc b)
)

; Let's reimplement the integer sum from a through b

(define (identity x)
    x)

(define (sum_int a b)
    (sum identity a inc b)
)

; Let's reimplement the pi-sum function analogously 

(define (pi-leibniz a b)
    (define (pi_sum a b)
        (define (pi-term x)
            (/ 1.0 (* x (+ x 2))))
        (define (pi-next x)
            (+ x 4))
        (sum pi-term a pi-next b)
    )
    (* 8 (pi_sum a b))
)

; Let's now take advantage of sum to implement an integral approximation based on the formula @page 107/883

; (sum Term a Next b)

(define (integral-A f a b dx)
    (define (small-inc x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) small-inc b)
       dx))









