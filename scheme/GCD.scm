; SICP greatest common divisor

; Implementation according to Euclid's Algorithm: GCD(a,b) = GCD (b, r), where r = remainder(a, b)

(define (GCD a b)
    (if (= b 0)
        a
        (GCD b (remainder a b)))
)