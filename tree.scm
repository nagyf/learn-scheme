#lang scheme

; Count the leaves of a tree
(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t))
                 (count-leaves (cdr t))))))

(define tree (cons (list 1 2) (list 3 4)))
(count-leaves tree)
(count-leaves (list tree tree))

; Reverse lists recursively
(define (deep-reverse xs)
  (define (deep-reverse-iter xs r)
    (cond ((null? xs) r)
          ((list? (car xs))
           (deep-reverse-iter (cdr xs) (cons (deep-reverse-iter (car xs) '()) r)))
          (else
           (deep-reverse-iter (cdr xs) (cons (car xs) r)))))
  (deep-reverse-iter xs '()))

(deep-reverse (list 1 2 (list 4 5 (list 7 8 9)) 10 11))

; List the leaf values of a tree
(define (fringe t)
  (define (fringe-iter ts rs)
    (cond ((null? ts) rs)
          ((pair? (car ts)) (fringe-iter (cdr ts) (fringe-iter (car ts) rs)))
          (else (fringe-iter (cdr ts) (append rs (list (car ts)))))))
  (fringe-iter t '()))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

; Mobile tree
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

; Returns the total weight of the tree
(define (total-weight mobile)
  (define (branch-weight b)
    (let ((bs (branch-structure b)))
      (cond ((pair? bs)
             (+ (branch-weight (left-branch bs)) (branch-weight (right-branch bs))))
            (else bs))))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; Checks if the tree is balanced
(define (balanced? m)
  (define (torque b)
    (let ((bs (branch-structure b))
          (bl (branch-length b)))
      (cond ((pair? bs) (* bl (+ (torque (left-branch bs))
                                 (torque (right-branch bs)))))
            (else (* bl bs)))))
  
  (= (torque (left-branch m))
     (torque (right-branch m))))

(define b (make-mobile (make-branch 4 5) (make-branch 5 6)))
(define b2 (make-mobile (make-branch 4 5) (make-branch 1 (make-mobile (make-branch 2 2) (make-branch 4 4)))))
(total-weight b)
(balanced? b)
(balanced? b2)

; Mapping over trees
(define (square x) (* x x))
(define (square-tree t)
  (cond ((null? t) '())
        ((pair? (car t)) (cons (square-tree (car t)) (square-tree (cdr t))))
        (else (cons (square (car t)) (square-tree (cdr t))))))

(define (tree-map f t)
  (cond ((null? t) '())
        ((pair? (car t)) (cons (tree-map f (car t)) (tree-map f (cdr t))))
        (else (cons (f (car t)) (tree-map f (cdr t))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(tree-map square
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

; Subsets
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (a) (append (list (car s)) a)) rest)))))
(subsets '(1 2 3))

; Fibonacci
(define (fib n)
  (define (fib-iter a b)
    (cond ((> a n) '())
          (else (cons a (fib-iter b (+ a b))))))
  (fib-iter 1 1))

(fib 21)

; A generic filter implementation
(define (filter pred seq)
  (cond ((null? seq) '())
        ((pred (car seq)) (cons (car seq)
                                (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

; A generic accumulation implementation
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

; Enumerate interval
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

; Enumerate a tree
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

;
; Some generic list manipulation expressed with higher order functions
;
(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length2 sequence)
  (accumulate (lambda (_ n) (+ n 1)) 0 sequence))

(map2 square '(1 2 3 4 5))
(append2 '(1 2 3 4 5) '(6 7 8 9 10))
(length2 '(1 2 3 4 5))


(define (sum x y . z)
  (define (add x y) (+ x y))
  (accumulate add 0 (append (list x y) z)))
    
; Horner's rule
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))