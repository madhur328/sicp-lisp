#lang racket

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (recursive n)
    (if (= n 1) 
        f
        (compose (recursive (- n 1)) f)))
  (recursive n))

(define (repeated-v2 f n)
  (define (recursive n)
    (if (= n 0) 
        (lambda (x) x)
        (compose f (recursive (- n 1)))))
  (recursive n))  

(define (repeated-v3 f n)
  (if (= n 0) 
      (lambda (x) x)
      (lambda (x) (f ((repeated-v3 f (- n 1)) x)))))

((compose square inc) 6)
((repeated square 2) 5)
((repeated-v3 square 2) 5)