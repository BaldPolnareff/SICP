; Sets as unordered lists of unique elements (no repetitions)

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((element-of-set? x set) set)
          (else (cons x set))))
(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))
(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
          (else (cons (car set1) (union-set (cdr set1) set2)))))

; SICP Exercise 2.60 
; Sets as unordered lists of repeatable elements 

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (append set1 set2))))
(define (adjoin-set x set)
    (cons x set))
; The others don't need to change

; Sets as ordered lists of unique elements

(define (element-of-set? x set)                     ; on average this requires n/2 steps, so still O(n)
    (cond ((null? set) false)
          ((= x (car set)) true)
          ((> (car set) x) false)
          (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)                ; going from O(n²) to O(n), a considerable speed gain
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
                   ((< x1 x2) (intersection-set (cdr set1) set2))
                   ((> x1 x2) (intersection-set set1 (cdr set2)))))))


; SICP Exercise 2.61 

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((= x (car set)) set)
          ((< x (car set)) (cons x set))
          ((> x (car set)) (cons (car set) 
                                 (adjoin-set x (cdr set))))))

; SICP Exercise 2.62

(define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else (let ((x1 (car set1)) (x2 (car set2)))
                        (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                              ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                              ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))


