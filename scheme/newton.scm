; implementing the average damping function as a procedure that returns a procedure

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (average a b)
    (/ (+ a b) 2))

(define (avg-damp fun)
    (lambda (x) (average x (fun x)))    
)

; square root procedure using average dump 

(define (sqr-root x tol)
    (fixed-point (avg-damp (lambda (y) (/ x y))) 1.0 tol)
)

; I want to generalize this concept by knowing that the root of order n of any given number can be iterated as a fixed point search, 
; just like the sqrt, which is the order 2: y -> x/y can be generalized as follows: 
; for n-root y -> x/y^(n-1)
; I can implement this making use of the fast-exp function I already implemented

(define (n-root x n tol)
    (fixed-point (avg-damp (lambda (y) (/ x (fast-exp y (- n 1))))) 1.0 tol))

; I'm observing that the higher the root order, the smaller the tolerance needed to have an acceptably precise convergence, this is probably 
; due to the average damping technique becoming less effective with roots of order n > 2, as I can quickly see 
; many more oscillations around the result, already with n = 3 

; The result already oscillates infinitely with n = 4 and a tolerance of 0.001, even a tolerance of 0.01 requires thousands of iterations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Newton's tangent method: x(k+1) = x(k) - fun(x(k))/fun'(x(k))

; definition of a derivative:                 lim (h->0) (f(x+h) - f(x))/h
; which, for h = dx << x is equivalent to:    f'(x) = (f(x+dx) - f(x))/dx

; I can implement a procedure that returns a procedure for the derivative, analogous to the average damp one
; and assuming f is differentiable, I can implement newton's tangent method as a fixed-point iteration until f(x) converges to 0
; x -> x - f(x)/f'(x)

(define (derivative f dx)
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (newton-transform f dx)
    (lambda (x) (- x (/ (f x) ((derivative f dx) x)))))

(define (newton-method fun guess dx tol)
    (fixed-point (newton-transform fun dx) guess tol))

; I can define the sqrt(x) as an application of newton's method for the function y-> y² - x with an initial guess of 1
; I will then compare that to the previous fixed-point iteration method:
;               (define (sqr-root x tol)
;                   (fixed-point (avg-damp (lambda (y) (/ x y))) 1.0 tol))

(define (sqrt-newt x dx tol)
    (newton-method (lambda (y) (- (square y) x)) 
                   1.0 dx tol))

; there doesn't seem to be a significative difference for the square root procedure with either fixed-point or newton
; however, it would be interesting to test the nweton method for the root of order n and compare it with the average damped
; fixed-point iteration method, assuming that the nth order root with newton's method can be mapped with y-> y^n -x

(define (nroot-newt x N dx tol)
    (newton-method (lambda (y) (- (fast-exp y N) x))
               1.0 dx tol))

; This procedure proved to be successful and converges fast (as expected by newton's method), compared to the average damped 
; fixed-point iteration, which oscillates indefinitely around the solution even with low accuracy!

; SICP Exercise 1.40

; f(x) = x³ + ax² + bx + c 

(define (cubic-newt a b c guess dx tol)
    (newton-method (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)) guess dx tol)
)

; SICP Exercise 1.41

; Deﬁne a procedure double that takes a pro-
; cedure of one argument as argument and returns a proce-
; dure that applies the original procedure twice. For exam-
; ple, if inc is a procedure that adds 1 to its argument, then
; (double inc) should be a procedure that adds 2. What
; value is returned by
; (((double (double double)) inc) 5)

(define (inc n)
    (+ n 1))

(define (double f-x)
    (lambda (x) (f-x (f-x x))))

; SICP Exercise 1.42

; Let f and g be two one-argument functions.
; The composition f after g is deﬁned to be the function x→f(g(x))
; Deﬁne a procedure compose that implements com-
; position. For example, if inc is a procedure that adds 1 to
; its argument,
; ((compose square inc) 6)
; 49

(define (compose f g)
    (lambda (x) (f (g x))))

; SICP Exercise 1.43 
; f(f(f....(f(x)))) n times

(define (repeat f n)
    (if (= n 1)
        f
        (compose f (repeat f (- n 1))))    
)

; SICP Exercise 1.44

; smooth-f(x) = avg(f(x-dx), f(x), f(x+dx)) dx<<<<x

(define (smooth f)
    (define dx 0.00001)
    (define (avg a b c)
        (/ (+ a b c) 3))
    (lambda (x) (avg (f (- x dx)) (f x) (f (+ x dx))))        
)

(define (smooth-n f n)
    ((repeat smooth n) f))

; SICP Exercise 1.45
; As I suspected, the average damping method can't make the fixed-point iteration converge to compute an nth root with n > 3
; Other than using newton (which is better anyway), the text suggests to use average damping multiple times and I'll experiment
; to determine how many dampings are necessary for any n
; I can take advantage of the just implemented (repeat f n) procedure

; IMPROVE THIS!
; (define (n-root x n tol)
;     (fixed-point (avg-damp (lambda (y) (/ x (fast-exp y (- n 1))))) 1.0 tol))


(define (n-root x n tol)
    (fixed-point (repeat (avg-damp (lambda (y) (/ x (fast-exp y (- n 1))))) (floor (log n 2))) 1.0 tol))

; The results still oscillates around the solution for (n-1) dampings
; There's convergence for n dampings, but the result is not accurate enough even after lots of iterations (testing only with n=4 at this point)
; it's very likely that the accuracy will drop with higher root orders and the amount of iterations will grow very fast
; Oddly enough, the result does not converge with (n+1) dampings
; No improvements with (n+2) dampings compared to n
; There's a noticeable but not particularly meaningful improvement with n² dampings
; After some research, it seems the ideal amount of dampings is log2(n), floored to the closest integer (floor (log n 2))
; Apparently this is the minimum value that guarantees convergence, but the result is not accurate enough even with thousands of iterations 
; (very small tol) regardless; Newton's method is definitely a much better option



