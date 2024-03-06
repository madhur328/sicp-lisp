#lang racket

(define (square x) (* x x))

(define (larger-two-squared x y z)
  (cond ((and (<= x y) (<= x z)) 
         (+ (square y) (square z)))
        ((and (<= y x) (<= y z)) 
         (+ (square x) (square z)))
        (else (+ (square x) (square y)))))

(larger-two-squared 1 2 3)        