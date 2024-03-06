#lang racket

(define (make-vect x y)
  (cons x y))

(define (x-cor-vect v)
  (car v))

(define (y-cor-vect v)
  (cdr v))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))
(define seg1 (make-segment v1 v2))
(start-segment seg1)
(end-segment seg1)