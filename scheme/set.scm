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

(define (make-account balance password)
    (define limit 7)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Porcoddio Mahmood Soldi"))
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)
    (define (call-the-cops)
        (error "Nino ninoooo stanno arrivando gli sbirri diocaneeee"))
    (define (print-attempts count limit)
        (display "Wrong password! Remaining attempts: ")
        (display (- limit count))
        (display " out of ")
        (display limit))

    (define (correct-password? input-pass)
        (cond ((equal? input-pass password) (reset-count (attempts-made)) true)
              (else (increase-count 1) false)))

    (define (increase-count)
        (make-accumulator 0))
    (define (reset-count)
        (lambda (counter) (set! counter 0)))
    (define (attempts-made)
        (increase-count 0))
    
    (define (dispatch pass m)
        (if (correct-password? pass (attempts-made))
                (cond ((equal? m 'withdraw) withdraw)
                      ((equal? m 'deposit) deposit)
                      (else (error "Unkwown operation - MAKE-ACCOUNT: " m)))
            (cond ((> (attempts-made) limit) (call-the-cops))
                  (else (print-attempts (attempts-made limit))))))
    dispatch
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rand (let ((x random-init))
                (lambda () (set! x (rand-update x)) x)))

(define (random-in-range inf sup)
    (let ((range (abs (- sup inf))))
        (+ inf (random range))))

(define (append! x y)
    (set-cdr! (last-pair x) y) x)
(define (make-cycle x)
    (set-cdr! (last-pair x) x) x)

(define z (make-cycle '('a 'b 'c)))

(define (bill-count-pairs x)
    (if (not (pair? x))
        0
        (+ (bill-count-pairs (car x))
           (bill-count-pairs (cdr x))
           1)))

(define cacca (list '(1 2) '(3 4) '(5 6)))

(define (count-pairs ls)
    )