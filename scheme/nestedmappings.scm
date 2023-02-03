(define (divides? a b)
    (= (modulo b a) 0))

(define (smallest-div n)
    (define (find-divisor n test-div)
        (cond ((> (square test-div) n) n)
            ((divides? test-div n) test-div)
            (else (find-divisor n (+ test-div 1)))))
    (find-divisor n 2))
(define (prime? n)
    (= n (smallest-div n)))

(define (flatmap proc ls)
    (fold-right append null (map proc ls)))

(define (permutations S)
    (if (null? S)
        (list null)
        (flatmap (lambda (x) (map (lambda (p) (cons x p)) (permutations (remove x S))))
                 S)))

; SICP Exercise 2.40

(define (unique-pairs n)
    (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n))
)

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (define (make-pair-sum pair)
        (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
    (map make-pair-sum (list-filter prime-sum? (unique-pairs n)))
)

; SICP Exercise 2.41

(define (unique-triples n)
    (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k)) 
                                               (enumerate-interval 1 (- i 1)))) 
                              (enumerate-interval 1 (- i 1)))) 
             (enumerate-interval 1 n))
)


(define (find-triples-sum-to-s n s)
    (define (make-triple-sum triple)
        (append triple (list (list-accumulate + 0 triple))))

    (define (triple-sum-to-s? triple)
        (= (list-ref (make-triple-sum triple) 3) s))

    (map make-triple-sum (list-filter triple-sum-to-s? (unique-triples n)))
)
    