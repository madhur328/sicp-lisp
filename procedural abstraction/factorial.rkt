#lang racket

(define (fact-rec n)
  (if (= n 1)
      1
      (* n (fact-rec (- n 1)))))


(define (fact-iter n)
  (define (iter result counter)
    (if (> counter n)
        result
        (iter (* result counter) (+ counter 1))))
  (iter 1 1))


(fact-rec 4)
(fact-iter 5)        