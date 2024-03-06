#lang racket

(require "expmod.rkt")

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (if (= times 0)
      #t
      (and (fermat-test n) (fast-prime? n (- times 1))))) 