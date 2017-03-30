#lang scheme

(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t))
                 (count-leaves (cdr t))))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
