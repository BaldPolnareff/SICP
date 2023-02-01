; SICP Exercise 2.37

(define (make-vector ls)
    (enumerate-tree ls))

(define (make-matrix v1 . vectors)
    (if (null? vectors)
        (make-vector v1)
        (cons v1 (list-accumulate cons null vectors))))

(define (extract-row matrix N)
    (if (null? matrix)
        null
        (list-ref matrix N)))

(define (dot-product v1 v2)
    (list-accumulate + 0 (map * v1 v2)))

(define (transpose m)
    (accumulate-n cons '() m)
)

(define (extract-column matrix N)
    (extract-row (transpose matrix) N)
)

; (define (matrix-*-vector m v)
;     (map (lambda (x) (dot-product x v)) m)
; )

(define (matrix-*-vector m v)
    (define (op x)
        (dot-product x v))
    (map op m)
)

(define (matrix-*-matrix m n)
    (define n_t (transpose n))
    (define (op x)
        (matrix-*-vector n_t x))
    (map op m)
)

; SICP Exercise 2.38

(define fold-right list-accumulate)

(define (fold-left op initial sequence)
    (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
    (iter initial sequence))

; op must be a commutative operator!

(define (commutative? op a b)
    (= (op a b) (op b a)))

; SICP Exercise 2.39

(define (reverse-ls ls)
    (fold-left (lambda (a b) (cons a b)) null ls)
)

(define (reverse-ls ls)
    (fold-right (lambda (a b) (cons b a)) null ls)
)