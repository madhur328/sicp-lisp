#lang racket

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))


(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (/ (- guess (improve guess)) guess)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  ((iterative-improve good-enough? improve) 1.0)
)

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) 0.00001))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))

(sqrt 16)
(fixed-point cos 1.0)