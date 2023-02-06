; SICP Exercise 2.54

(define (Equal? ls1 ls2)
    (cond ((and (null? ls1) (null? ls2)) true)
          ((and (pair? ls1) (pair? ls2))
                (and (Equal? (car ls1) (car ls2))
                     (Equal? (cdr ls1) (cdr ls2))))
          ((and (number? ls1) (number? ls2)) (= ls1 ls2))
          ((or (pair? ls1) (pair? ls2)) false)
          (else (eq? ls1 ls2))))



