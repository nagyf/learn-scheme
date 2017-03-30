#lang scheme

(define (cube a) (* a a a))

;;; A generic summation higher-order procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity a) a)
(define (inc n) (+ n 1))
(define (sum-cubes a b) (sum cube a inc b))
(define (sum-integers a b) (sum identity a inc b))

;;; The pi-sum gives an approximation to Pi
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 10000))

;;; Definitive integral
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.001)

;;; Product
(define (product f a next b)
  (cond ((> a b) 1)
        (else (* (f a) (product f (next a) next b)))))

; Factorial defined with the product higher order function
(define (factorial x)
  (product * 1 inc x))
(factorial 5)

; Pi approximation with Wallis product
(define (inc2 x) (+ x 2))
(define (square x) (* x x))
(define (approximate-pi n)
  (define (term-left x) (/ (* 2 x) (- (* 2 x) 1)))
  (define (term-right x) (/ (* 2 x) (+ (* 2 x) 1)))
  (define (term x) (* (term-left x) (term-right x)))
  (* 2.0 (product term 1 inc n)))

(approximate-pi 1000)

;;; The more general Accumulate
(define (accumulate combiner null-value f a next b)
  (cond ((> a b) null-value)
      (else (combiner (f a) (accumulate combiner null-value f (next a) next b)))))

(define (product2 f a next b)
  (accumulate * 1 f a next b))
(product2 identity 1 inc 5)

(define (sum2 f a next b)
  (accumulate + 0 f a next b))
(sum2 identity 1 inc 10)