#lang racket

(provide prime?)

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))

(define (smallest-divisor-v2 n)
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (next test-divisor)))))
  (find-divisor 2))  

(define (prime? n)
  (= n (smallest-divisor n)))  

(smallest-divisor 199)    ; 199
(smallest-divisor 1999)   ; 1999
(smallest-divisor 19999)  ; 7