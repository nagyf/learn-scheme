#lang scheme

; Identity function
(define (id x) x)

; Incerements the argument by 1
(define (inc x) (+ x 1))

; Decrements the argument by 1
(define (dec x) (- x 1))

; Create list of numbers in the range of n (inclusive) and m (exclusive)
(define (range n m)
  (cond ((= n m) '())
        ((< n m) (cons n (range (inc n) m)))
        ((> n m) (cons n (range (dec n) m)))))

; Maps a function over a list of things
(define (map f xs)
  (cond ((empty? xs) '())
        (else (cons (f (car xs)) (map f (cdr xs))))))

; Reduce a list into a single value by the function f and accumulator a
(define (reduce f xs a)
  (cond ((empty? xs) a)
        (else (reduce f (cdr xs) (f a (car xs))))))

; The same as reduce but using the first element as the accumulator
(define (reducel f xs)
  (reduce f (cdr xs) (car xs)))

; Filters the list by the specified predicate f
(define (filter f xs)
  (cond ((empty? xs) '())
        ((f (car xs)) (cons (car xs) (filter f (cdr xs))))
        (else (filter f (cdr xs)))))

;;;
;;; Testing the functions
;;;

(range 15 -2)
(map (lambda (x) (* x x)) '(1 2 3 4 5))
(reduce (lambda (x y) (* x y)) '(1 2 3 4 5) 1)
(reducel (lambda (x y) (* x y)) '(1 2 3 4 5))
(filter (lambda (x) (= 0 (remainder x 2))) (range 1 10))
