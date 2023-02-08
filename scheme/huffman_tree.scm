(define (make-leaf symbol wt)
    (list 'leaf symbol wt))
(define (leaf? object)
    (equal? (car object) 'leaf))
(define (symbol-leaf x)
    (cadr x))
(define (weight-leaf x)
    (caddr x))

(define (make-code-tree left right)
    (list left
          right
          (append (symbols left)
                  (symbols right))
          (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch  branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit in CHOOSE BRANCH: only 0 or 1 allowed" bit))))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
                    (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))


(define (adjoin-set x set)
    (cond ((null? set) '())
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair) (cadr pair))
            (make-leaf-set (cdr pairs))))))
; SICP Exercise 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message `(0 1 1 0 0 1 0 1 0 1 1 1 0))
; '(A D A B B C A)

; SICP Exercise 2.68

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (entry set)) true)
          ((< x (entry set)) (element-of-set? x (left-branch set)))
          ((> x (entry set)) (element-of-set? x (right-branch set)))))


(define (encode-symbol symbol tree)
    (cond ((leaf? tree) '())
          ((memq symbol (symbols (left-branch tree)))
                (cons 0 (encode-symbol symbol (left-branch tree))))
          ((memq symbol (symbols (right-branch tree)))
                (cons 1 (encode-symbol symbol (right-branch tree))))
          (else (error "Input symbol not present in tree - ENCODE-SYMBOL: " symbol))))

; SICP Exercise 2.69

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (successive-merge (adjoin-set ) 
                          ())))