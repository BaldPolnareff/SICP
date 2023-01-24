(define (square x)
    (* x x));
(define (square_sum x y)
    (+ (square x) (square y)));

(define (pythagoras x y)
    (sqrt (square_sum x y)));

(define (factorial x)
(cond ((= x 0) 1)
    (else (* x (factorial (- x 1))))))


;Implement an iterative factorial that uses a counter and stores the latest product, as it's more memory efficient

;Implementing a counter function as a helper

(define (ifactorial_helper product counter n)
    (if (> counter n)
        product
        (ifactorial_helper (* product counter) (+ counter 1) n)))

;Now implementing the function that iterates through the helper

(define (ifactorial x)
    (ifactorial_helper 1 1 x))

;IMPLEMENT AN EXPONENTIAL FUNCTION a^b BOTH RECURSIVELY AND ITERATIVELY

;Recursive implementation
(define (exponential x y)
    (if (= y 0)
        1
        (* x (exponential x (- y 1)))))

;Iterative implementation 

(define (exp_helper product base exponent)
    (if (= exponent 0)
        product
        (exp_helper (* base product) base (- exponent 1) )))

(define (iexponential x y)
    (exp_helper 1 x y))

;SICP exercise 1.3: Deﬁne a procedure that takes three numbers as arguments and returns the sum of the squares of the two
;larger numbers.

    ;decomposition: calculate the two larger numbers first, then square them, then sum 

;make a function that takes the minimum value and returns all other

(define (square x)
    (* x x));
(define (square_sum x y)
    (+ (square x) (square y)));

(define (sum-square_two-major x y z)
    (if (= x (min x y z))
        (square_sum y z) 
        (square_sum (x (max y z))))
)

;Implement Newton's method for square roots

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x tol)
    (< (abs (- (square guess) x)) tol)
)

(define (improve-guess guess x)
    (average guess (/ x guess))    
)

(define (newton-sqrt-iter guess x tol)
    (if (good-enough? guess x tol)
        guess
        (newton-sqrt-iter (improve-guess guess x) x tol))    
)

(define (error-checker x y)
    (abs (- x y))    
)

;SICP Exercise 1.8: Implement Newton's method for cubic roots


(define (improve-guess-cubic guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)
)

(define (newton-cuberoot-iter guess x tol)
    (if (good-enough? guess x tol)
        guess
        (newton-cuberoot-iter (improve-guess-cubic guess x) x tol)
    )
)

; Fibonacci: 0, 1, 1, 2, 3, 5, 8, 13, 21... 0 if x = 0, 1 if x = 1, Fib(x-2) + Fib(x-1) if x>1

;Recursive implementation: very slow! the space grows linearly but the time grows exponentially! (Fibonacci 44 takes several seconds already)
(define (Fibonacci x)
    (cond ((= x 0) 0)
        ((= x 1) 1)
        (else (+ (Fibonacci (- x 2)) 
                 (Fibonacci (- x 1))))
    )
)

;Iterative implementation: it grows linearly in time and the output is instantaneous even with large numbers

(define (fib-iter a b counter)
    (if (= counter 0)
        b
        (fib-iter (+ a b) a (- counter 1)))
)

(define (ifib n)
    (fib-iter 1 0 n)
)

; SICP Exercise 1.11

; recursive form: super easy, extremely slow
(define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1)) 
           (* 2 (f (- n 2))) 
           (* 3 (f (- n 3)))
        )
    )
)

; iterative implementation: 

(define (iter-f a b c counter)
    (if (< counter 3)
        counter
        (iter-f (+ a (* 2 b) (* 3 c)) 
                a 
                b 
                (- counter 1)
        )
    )
)

(define (i-f n)
    (iter-f 2 1 0 n)
)

