#lang racket

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (/ (- guess (improve guess)) guess)) 0.0001))
  (sqrt-iter 1.0))

(sqrt 2.0)