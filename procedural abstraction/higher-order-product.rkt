#lang racket

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) (product-rec term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
       result
       (iter (next a) (* result (term a)))))
  (iter a 1))

(define (square x)
  (* x x))

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (factorial n)
  (product-iter identity 2 inc n))

(define (pi-product a b)
  (define (pi-term x)
    (/ (* x (+ x 2)) (square (+ x 1))))
  (define (pi-next x)
   (+ x 2))
  (product-iter pi-term a pi-next b))

  
(factorial 5)
(* 4 (pi-product 2.0 1000.0))