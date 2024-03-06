#lang racket

(define (square x)
  (* x x))

(define (cube-root x)
  (define (cube-root-iter guess)
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve guess))))
  (define (good-enough? guess)
    (< (abs (/ (- guess (improve guess)) guess)) 0.0001))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (cube-root-iter 1.0))

(cube-root 8.0)  