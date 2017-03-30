#lang scheme

(define (make-interval a b) (cons a b))

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (lower-bound b))
                 (- (upper-bound a) (upper-bound b))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (cond ((or (= 0 (upper-bound y)) (= 0 (lower-bound y))) (error "Division by zero interval"))
        (else (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(make-interval 5.8 7.6)
(add-interval (make-interval 4 6) (make-interval 7 9))
(sub-interval (make-interval 9 11) (make-interval 3 4))
(width (make-interval 4 8))

; Excercise 2.9
(let ((a (make-interval 4 8)) (b (make-interval 6 7)))
  (= (width (add-interval a b)) (+ (width a) (width b))))
(let ((a (make-interval 4 8)) (b (make-interval 6 7)))
  (= (width (sub-interval a b)) (- (width a) (width b))))

; Excercise 2.9
(let ((a (make-interval 4 8)) (b (make-interval 6 7)))
  (= (width (mul-interval a b)) (* (width a) (width b))))
(let ((a (make-interval 4 8)) (b (make-interval 6 7)))
  (= (width (div-interval a b)) (/ (width a) (width b))))

; Excercise 2.11
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (let ((percentage (* (/ c 100) p)))
    (make-interval (- c percentage)
                   (+ c percentage))))

(define (diff i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (/ (center i) (diff i)))

(make-center-percent 60 10)
(percent (make-center-percent 60 10))

