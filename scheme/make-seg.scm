; SICP Exercise 2.2

(define (make-pt x y)
    (cons x y))

(define (x-coord P)
    (car P))
(define (y-coord P)
    (cdr P))

(define (dist P1 P2)
    (let ((deltax (abs (- (x-coord P1) (x-coord P2)))) (deltay (abs (- (y-coord P1) (y-coord P2)))))
        (sqrt (+ (square deltax) (square deltay)))))

(define (make-seg P1 P2)
    (cons P1 P2))

(define (seg-start seg)
    (car seg))

(define (seg-end seg)
    (cdr seg))

(define (print-point P)
    (newline)
    (display "(x, y) = ")
    (display "(")
    (display (x-coord P))
    (display ", ")
    (display (y-coord P))
    (display ")"))

(define (print-seg seg)
    (define P1 (seg-start seg))
    (define P2 (seg-end seg))
    (define Len (dist P1 P2))
    (display "P1: ")
    (print-point P1)
    (newline)
    (display "P2: ")
    (print-point P2)
    (newline)
    (display "Length: ")
    (display Len))