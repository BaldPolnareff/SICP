; SICP Primality tests

; First algorithm searches for the smallest integral divisor in a sequence of integers and verifies the primality 
; with a custom predicate (prime?), the order of growth is Theta=(sqrt(n))

; if n NOT prime => divisor <= sqrt(n) ======> test only integers between 1 and sqrt(n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; custom predicate to check divisibility
(define (divides? a b)
    (= (remainder b a) 0)
);
(define (square x)
    (* x x));

; auxiliary function that finds a divisor between 1 and sqrt(n) by integral increments of + 1
(define (find-divisor n test-div)
    (cond ((> (square test-div) n) n)
        ((divides? test-div n) test-div)
        (else (find-divisor n (+ test-div 1)))
    )
);
; smallest divisor is defined between 1 and sqrt(n) with 1 excluded, so the test always starts with 2 and there's 
; no need to input a test divisor if we always assume that starting point, 
; so (smallest-div n) is just a convenient wrapper for (find-divisor n test-div) 
(define (smallest-div n)
    (find-divisor n 2)
);

(define (prime? n)
    (= n (smallest-div n))
);
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following implementation is based on Fermat's Little Theorem and Theta=log(n)
; Fermat's Little Theorem: 
; If n is a prime number and any integer a is such that 0 < a < n, 
; Then a^n ⩭ a*mod(p)

; In simpler terms, (divides? (a^n - a) n) == #t

(define (square x)
    (* x x));

(define (exp_helper product base exponent)
    (if (= exponent 0)
        product
        (exp_helper (* base product) base (- exponent 1) )));

(define (iexp x y)
    (exp_helper 1 x y));

(define (even? n)
    (if (= n 0)
        #f
        (= (remainder n 2) 0)) 
);

(define (fast-exp x y)
    (if (even? y)
        (square (fast-exp x (/ y 2)))
        (iexp x y))
);

; We need to create a custom function that computes the exponential of a modulo b
; modulo operation: a%b = (remainder a b)
; (a^b)%m is a general operation of expmod for a base a, an exponent b and a modulo m
; The modulo operation is already built-in and it should be identical to remainder (not sure)

(define (expmod base expo M)
    (modulo (fast-exp base expo) M)
);

; Assuming I have an integer n to check its primality, the fermat test requires any random number 
; 1 < a <= (n-1) to be equal to (expmod a n n)

(define (fermat-test n)
    (define (trial a)
        (= (expmod a n n) a))
    (trial (+ 1 (random(- n 1))))
)

; The primality given by the fermat-test is not a certain answer, but rather a probabilistic one:
; A test failure guarantees that n is not prime, however any number of successful trials only increases 
; the likelyhood that n is prime, without certainty

(define (prob-prime? n N)
    (cond ((= N 0) #t)
        ((fermat-test n) (prob-prime? n (- N 1)))
        (else #f))
)

(define (timed_prob-time? n N)
    (newline)
    (display "Tested integer: ")
    (display n)
    (newline)
    (display "Number of tests: ")
    (display N)
    (newline)
    (time (prob-prime? n N))
    (display " ms")
)
; ##########################################################################################################

; Improve the smallest-divisor function knowing that once divisibility by 2 has been confirmed, there's no point in 
; checking the divisibility for any even number other than two: this should reduce the test to checking divisibility 
; by 2 and then by all odd numbers

; ##########################################################################################################
; OLD PROCEDURE
; auxiliary function that finds a divisor between 1 and sqrt(n) by integral increments of + 1
(define (find-divisor n test-div)
    (cond ((> (square test-div) n) n)
        ((divides? test-div n) test-div)
        (else (find-divisor n (+ test-div 1)))
    )
);
; smallest divisor is defined between 1 and sqrt(n) with 1 excluded, so the test always starts with 2 and there's 
; no need to input a test divisor if we always assume that starting point, 
; so (smallest-div n) is just a convenient wrapper for (find-divisor n test-div) 
(define (smallest-div n)
    (find-divisor n 2)
);
; ##########################################################################################################

; defining an auxiliary function that given a test divisor, returns the next one

(define (next-div test-div)
    (if (= test-div 2)
        3
        (+ test-div 2))
);

(define (find-divisor n test-div)
    (cond ((> (square test-div) n) n)
        ((divides? test-div n) test-div)
        (else (find-divisor n (next-div test-div)))
    )
);

