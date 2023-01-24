; SICP Exercise 1.33

(define (square x)
    (* x x))

(define (prime? n)
    (define (divides? a b)
        (= (remainder b a) 0))
    (define (find-divisor x test-div)
        (cond ((> (square test-div) x) x)
            ((divides? test-div x) test-div)
            (else (find-divisor x (+ test-div 1)))
        )
    )
    (define (smallest-div m)
        (find-divisor m 2))
    (= (smallest-div n) n))

(define (accumulate-filter operation cond? nullv f a next b)
    (define (iter a result)
        (if (> a b)
            result
            (if (not (cond? a))
                (iter (next a) result)
                (iter (next a) (operation (f a) result)))
        )       
    )
    (iter a nullv)
)

; the sum of the squares of the prime numbers in the
; interval a to b (assuming that you have a prime? pred-
; icate already written)

(define (isum-filter f a next b cond?)
    (accumulate-filter + cond? 0 f a next b))

(define (iprod-filter f a next b cond?)
    (accumulate-filter * cond? 1 f a next b))

(define (squareprime-sum a b)
    (isum-filter square a inc b prime?))

; the product of all the positive integers less than n that
; are relatively prime to n (i.e., all positive integers i < n
; such that GCD(i, n) = 1)

(define (identity x)
    x)

(define (relative-prime? i)
    (= (GCD i n) 1)
)

(define (relprime-prod n)
    (define (relative-prime? i)
        (= (GCD i n) 1))
    (iprod-filter identity 1 inc n relative-prime?)
)