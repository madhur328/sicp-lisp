#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (recursive n)
    (if (= n 1) 
        f
        (compose (recursive (- n 1)) f)))
  (recursive n))

(define dx 0.00001)

(define (average3 x y z) (/ (+ x y z) 3))

(define (smooth f)
  (lambda (x) (average3 (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))