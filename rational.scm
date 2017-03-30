#lang scheme

; Checks if a number is negative
(define (neg? x)
  (< x 0))

; Make a number negative
(define (neg x)
  (cond ((neg? x) x)
        (else (- x))))

(define (normalize x y)
  (cond ((and (neg? x) (neg? y)) (cons (abs x) (abs y)))
        ((or (neg? x) (neg? y)) (cons (neg x) (abs y)))
        (else (cons x y))))

; Create a rational number
(define (make-rat n d)
  (let* ((divisor (gcd n d)) (nn (/ n divisor)) (dd (/ d divisor)))
    (normalize nn dd)))

; Simplifies a rational number
(define (simplify x)
  (let ((divisor (gcd (numer x) (denom x))))
    (make-rat (/ (numer x) divisor)
              (/ (denom x) divisor))))

; Greatest common divisor
(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))

; Returns the numerator
(define (numer x) (car x))

; Returns the denominator
(define (denom x) (cdr x))

; Prints the rational number
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Adds two ration numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

; Subtracts two rational numbers
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

; Multiplies two rational numbers
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

; Divides two rational numbers
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

; Checks if the two rational numbers are equal
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(add-rat (make-rat 1 2) (make-rat 2 4))
(sub-rat (make-rat 3 4) (make-rat 2 4))
(mul-rat (make-rat 3 6) (make-rat 3 6))
(equal-rat? (make-rat 1 2) (make-rat 2 4))
(simplify (make-rat 8 16))