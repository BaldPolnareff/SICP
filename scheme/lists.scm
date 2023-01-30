; SICP Exerxise 2.17

(define (last-pair items)
    (if (null? (cdr items))
        items
        (last-pair (cdr items))))

; SICP Exercise 2.18

(define (reverse-list items)
    (define (reverse-iter items reversed)
        (if (null? items)
            reversed
            (reverse-iter (cdr items) (cons (car items) reversed)))    
    )
    (reverse-iter items '())
)

; SICP Exercise 2.20

(define (even? N)
    (= (modulo N 2) 0))
(define (odd? N)
    (not (even? N)))

(define (parity? n m)
    (or (and (even? n) (even? m))
        (and (odd?  n) (odd?  m))))

(define (inc N) (+ N 1))


(define (maplist proc L)
    (if (null? L)
        '()
        (cons (proc (car L))
              (maplist proc (cdr L)))))

(define (each-element proc L)
    (cond ((null? L) '())
        (else (display (proc (car L))) 
              (newline)
              (display (each-element proc (cdr L))))))
