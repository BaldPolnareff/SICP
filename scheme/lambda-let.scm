; lambda expression and let 

; f(x, y) = x(1+xy)² + y(1-y) + (1+xy)(1-y)
; a = (1+xy)
; b = (1-y)
; => f = xa² + yb + ab

; implementing this function with internal definitions, as usual

(define (F x y)
    (define (helper a b)
        (+ (* x (square a)) (* y b) (* a b))    
    )
    (helper (+ 1 (* x y)) (- 1 y))
)

; let's now use the lambda expression instead
; (define (square x) (* x x)) == (define square (lambda (x) (* x x)))

(define (f x y)
    ((lambda (a b) 
        (+ (* x (square a)) 
           (* y b) 
           (* a b))) 
        (+ 1 (* x y)) 
        (- 1 y)
    )
)

; let's now use the let expression, which is a special form of lambda that's way more convenient to define local variables
; (let (    (<v1>) (<e1>)
;           (<v2>) (<e2>)f
;                ...
;           (<vn>) (<en>)) 
;      (<body>)
; )

(define (g x y)
    (let ((a (+ 1 (* x y)))
           (b (- 1 y)))
        (+ (* x (square a))
           (* y b)
           (* a b)
        ))
)