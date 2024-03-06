#lang racket

(define (mul-rec a b)
  (if (= b 0)
      0
      (+ a (mul-rec a (- b 1)))))

(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2)) 

(define (fast-mul-rec a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul-rec a (halve b))))
        (else (+ a (fast-mul-rec a (- b 1))))))

(define (fast-mul-iter a b)
  (define (iter a b result)
    (cond ((= b 0) result)
          ((even? b) (iter (double a) (halve b) result))
          (else (iter a (- b 1) (+ a result)))))
  (iter a b 0))

(fast-mul-iter 5 20)         