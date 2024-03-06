#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (average x y)
  (/ (+ x y) 2.0))

(define (midpoint-segment seg)
  (let ((mid-x (average (x-point (start-segment seg)) 
                        (x-point (end-segment seg))))
        (mid-y (average (y-point (start-segment seg)) 
                        (y-point (end-segment seg)))))
    (make-point mid-x mid-y)))

(define A (make-point 2 4))
(x-point A)
(y-point A)
(define B (make-point 5 8))
(x-point B)
(y-point B)
(define AB (make-segment A B))
(print-point (start-segment AB))
(print-point (end-segment AB))
(define M (midpoint-segment AB))
(print-point M)