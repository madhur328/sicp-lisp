#lang racket

(define (make-vect x y)
  (cons x y))

(define (x-cor-vect v)
  (car v))

(define (y-cor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (x-cor-vect v1) 
                (x-cor-vect v2))
             (+ (y-cor-vect v1) 
                (y-cor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (x-cor-vect v1) 
                (x-cor-vect v2))
             (- (y-cor-vect v1) 
                (y-cor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (x-cor-vect v))
             (* s (y-cor-vect v))))

(define v1 (make-vect 2 4))                
(define v2 (make-vect 1 2))
(x-cor-vect v1)               
(y-cor-vect v1)
(add-vect v1 v2)                
(sub-vect v1 v2)                
(scale-vect 2 v1)               