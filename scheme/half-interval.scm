; Implement the half interval method: 
; For a continuous function f, given an interval [a, b], if f(a) < 0 < f(b), then there MUST be an x such that f(x) = 0
; let m = (a + b)/2; 
; xk = m;
; if f(xk) > 0 then x(k+1) in [a, m] else in [m, b]
; iterate until length(interval) < tol

(define (rangelen a b)
    (abs (- b a)))

(define (close-enough? N tol)
    (< (abs N) tol))

(define (average a b) (/ (+ a b) 2))

(define (half-interval fun inf sup tol)
      (let ((m (average inf sup)))
          (if (close-enough? (rangelen inf sup) tol)
              m
              (let ((test (fun m)))
                  (cond ((positive? test) (half-interval fun inf m tol))
                        ((negative? test) (half-interval fun m sup tol))
                        (else m)))))
)

(define (bisection fun inf sup tol)
    (let ((f-a (fun inf))
          (f-b (fun sup))  
         )

            (cond ((and (negative? f-a) (positive? f-b)) 
                        (half-interval fun inf sup tol))
                  ((and (negative? f-b) (positive? f-a)) 
                        (half-interval fun sup inf tol))
                (else (error "f(a) < 0 < f(b) not met for (a b) =" inf sup))
            )
    )
)

(define (testfun x)
    (- (* x x x) (* 2 x) 3)
)