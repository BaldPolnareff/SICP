
; installing polyomials

(define (install-polynomial-package)
; internal procedures
; representation of poly
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (variable? x) (symbol? x))
    (define (same-variable? x1 x2)
        (and (variable? x1) (equal? x1 x2)))
    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
            (error "Polys are not in same var - ADD-POLY:" (list p1 p2))))
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
            (error "Polys are not in same var - MUL-POLY:" (list p1 p2))))
            (define (add-terms L1 L2)
                (cond ((empty-termlist? L1) L2)
                    ((empty-termlist? L2) L1)
                    (else
                        (let ((t1 (first-term L1))
                            (t2 (first-term L2)))
                            (cond ((> (order t1) (order t2))
                            (adjoin-term
                                t1 (add-terms (rest-terms L1) L2)))
                                              ((< (order t1) (order t2))
                                               (adjoin-term
                                                t2 (add-terms L1 (rest-terms L2))))
                                                    (else
                                                        (adjoin-term
                                                            (make-term (order t1)
                                                                       (add (coeff t1) (coeff t2)))
                                                            (add-terms (rest-terms L1)
                                                                       (rest-terms L2)))))))))
)