(define balance 100)

(define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))

(define new-withdraw
    (let ((balance 100))
        (lambda (amount) (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Insufficient funds"))))

(define (make-withdraw balance)
    (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Porcoddio Mahmood Soldi")))

(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Porcoddio Mahmood Soldi"))
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)
    (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request - MAKE-ACCOUNT: " m))))
    dispatch)

; SICP Exercise 3.1

(define (make-accumulator initial)
    (let ((accumulator initial))
        (lambda (x) (set! accumulator (+ x accumulator)) accumulator)))

; SICP Exercise 3.2

(define (make-monitored fun)
    (let ((counter 0))
        (define (dispatch mf)
            (cond ((equal? mf 'how-many-calls?) counter)
                  ((equal? mf 'reset-count) (set! counter 0))
                  (else (begin (set! counter (inc counter)) (fun mf)))))
    dispatch))

; SICP Exercise 3.3

(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Porcoddio Mahmood Soldi"))
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)
    (let ((count 0))
        (define (dispatch pass m)
            (if (equal? pass password)
                (cond ((equal? m 'withdraw) withdraw)
                      ((equal? m 'deposit) deposit)
                      (else (error "Unknown operation - MAKE-ACCOUNT: " m)))
                (begin (set! count (+ count 1))
                       (when (> count 7)
                            (call-the-cops))
                       (error "Too many attempts (out of 7), remaining: " (- 7 count))))))
    dispatch)

(define (call-the-cops)
    (error "Nino nino porcoddioooooooo"))

(define (increment n)
    ((make-accumulator n) 1))
