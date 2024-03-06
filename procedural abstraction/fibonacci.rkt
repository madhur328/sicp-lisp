#lang racket

(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

;linear iterative
(define (fib-iter n)
  (define (iter a b counter)
    (if (= counter n)
        b
        (iter (+ a b) a (+ counter 1))))
  (iter 1 0 0))

;logrithmic iterative

(define (fib-iter-v2 n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (* p p) (* q q))
                 (+ (* q q) (* 2 p q))
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))

(fib-iter-v2 2)  
(fib-iter-v2 3)
(fib-iter 16)  
(fib-iter-v2 16)  