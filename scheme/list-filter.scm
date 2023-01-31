(define (list-filter predicate? seq)
    (cond ((null? seq) null)
          ((predicate? (car seq)) 
                  (cons (car seq) 
                        (list-filter predicate? (cdr seq))))
          (else   (list-filter predicate? (cdr seq)))))

(define (square? x)
    (integer? (sqrt x)))

(define (list-accumulate op start ls)
    (if (null? ls)
        start
        (op (car ls)
            (list-accumulate op start (cdr ls)))))

(define (enumerate-interval inf sup)
    (if (< sup inf)
        null
        (cons inf (enumerate-interval (inc inf) sup)))
)

(define (enumerate-tree tree)
    (cond ((null? tree) null)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree)))))
)

(define (sum-odd-squares tree)
    (list-accumulate + 0 (maplist square (list-filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
    (list-accumulate cons '() (maplist fib (list-filter even? (enumerate-interval 0 n)))))

(define (horner-eval x coeff-list)
    (define (horner-iter a-i higher-terms)
        (+ a-i (* higher-terms x)))
    (list-accumulate horner-iter 0 coeff-list))

(define (accumulate-n op start ls-seq)
    (if (null? (car ls-seq))
        null
        (cons (list-accumulate op start (maplist car ls-seq))
              (accumulate-n    op start (maplist cdr ls-seq)))))

