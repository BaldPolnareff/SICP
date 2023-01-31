; SICP Exercise 2.37

(define (make-vector ls)
    (enumerate-tree ls))

(define (make-matrix v1 . vectors)
    (if (null? vectors)
        (make-vector v1)
        (cons v1 (list-accumulate cons null vectors))))

(define (dot-product v1 v2)
    (list-accumulate + 0 (map * v1 v2)))