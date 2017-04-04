#lang scheme

(define (last-pair xs)
  (cond ((null? xs) '())
        ((null? (cdr xs)) (car xs))
        (else (last-pair (cdr xs)))))

(define (reverse xs)
  (define (reverse-iter xs r)
    (cond ((null? xs) r)
          (else (reverse-iter (cdr xs) (cons (car xs) r)))))
  (reverse-iter xs '()))

(last-pair '())
(last-pair (list 1 2 3 4 5 6))
(reverse (list 1 2 3 4 5 6))