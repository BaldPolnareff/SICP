(define (count-leaves L)
    (cond ((null? L) 0)
          ((not (pair? L)) 1)
          (else (+ (count-leaves (car L))
                   (count-leaves (cdr L))))))

; SICP Exercise 2.25
; Give combinations of cars and cdrs that
; will pick 7 from each of the following lists: 
; 
; (1 3 (5 7) 9)
; ((7))
; (1 (2 (3 (4 (5 (6 7))))))

(define A '(1 3 (5 7) 9))
(define B '((7)))
(define C '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr A))))) ; ✓
(car (car B))                   ; ✓
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr C)))))))))))) ; ✓

; SICP Exercise 2.27

; (define (deep-reverse ls)
;     (define (iter items target)
;         (if (null? items)
;             target
;             (let ((next (car items)))
;                 (iter (cdr items)
;                       (cons 
;                        (if (pair? next)
;                             (deep-reverse next)
;                             next)
;                        target))))
;     )
;     (iter ls list)
; )
(define (append-list L1 L2)
    (if (null? L1)
        L2
        (cons (car L1) (append-list (cdr L1) L2)))
)

(define (deep-reverse L)
    (cond ((null? L) 
                  L)
          ((pair? (car L)) 
                (append-list (deep-reverse (cdr L)) (list (deep-reverse (car L)))))
          (else (append-list (deep-reverse (cdr L)) (list (car L)))))
)

; SICP Exercise 2.28

; (define (display-leaves L)
;     (cond ((null? L) null)
;           ((not (pair? L)) L)
;           (else (cons (list (display-leaves (car L)))
;                         (list (display-leaves (cdr L)))))))
