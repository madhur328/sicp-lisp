#lang racket

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (iter a b c n)
    (if  (= n 0)
         a
         (iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))
  (iter 0 1 2 n))              