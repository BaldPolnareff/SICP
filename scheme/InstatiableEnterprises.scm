; SICP Exercise 2.74

(define (make-network-file division file)
    (cons division file))
(define (file-division network-file)
    (car network-file))
(define (original-file network-file)
    (cdr network-file))
(define (get-record employee network-file)
    body)