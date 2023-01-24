; SICP Exercise 1.29: Simpson's rule for integration

; int(f(x), a, b) = h/3 * (y0 + 4y1 + 2y2 + 4y3 + 2y4 +...+ 2y(n-2) + 4y(n-1) + yn)
; where h = (b - a)/n     (increasing n increases the approximation's precision)
; for n an even integer
; and for yk = f(a + kh)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define a procedure that takes as input f, a, b, n and outputs the integral using Simpson's rule
; let's rearrange the sum by grouping odd and even terms
; h/3 * [y0 + yn + 4*(y1 + y3 +...+ y(n-1)) + 2*(y2 + y4 +...+ y(n-2))]

(define (sum Function a Next b)
    (if (> a b)
        0
        (+ (Function a)
           (sum Function (Next a) Next b)
        ))
)

(define (integral-simpson f a b n)
    (define h
        (/ (- b a) n))
    (define (next x)
        (+ x (* 2.0 h)))
    (*  (+ (f a) 
           (* 2 (sum f    a    next b)) 
           (* 4 (sum f (+ a h) next b)) 
           (f b))
        (/ h 3.0)        
    )
)