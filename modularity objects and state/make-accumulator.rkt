#lang racket

(define (make-accumulator1 initial-value)
  (define (accumulate amount)
    (set! initial-value (+ amount initial-value))
    initial-value)
  accumulate)

(define (make-accumulator2 value)
  (lambda (change) 
    (begin (set! value (+ value change)) value)))

(define A (make-accumulator1 5))

(A 10)
(A 10)