#lang scheme

(define (make-point x y) (cons x y))
(define (point-x p) (car p))
(define (point-y p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (point-x p))
  (display ",")
  (display (point-y p))
  (display ")"))

(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (make-point (/ (+ (point-x (start-segment s)) (point-x (end-segment s))) 2)
              (/ (+ (point-y (start-segment s)) (point-y (end-segment s))) 2)))

(midpoint-segment
 (make-segment (make-point 2 3)
               (make-point 13 7)))
