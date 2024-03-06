#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result) 
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a) 
         (product-rec term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
       result
       (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x)
  x)

(define (inc x)
  (+ x 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(accumulate * 1 identity 2 inc 6)
(accumulate + 0 identity 1 inc 10)
(accumulate-iter * 1 identity 2 inc 6)
(accumulate-iter + 0 identity 1 inc 10)