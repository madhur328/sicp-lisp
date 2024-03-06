#lang racket

(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

(define (expt-iter b n)
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (- counter 1) 
              (* b product))))
  (iter n 1))

(define (square x)
  (* x x))  

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-expt-rec b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-rec b (/ n 2))))
        (else (* b (fast-expt-rec b (- n 1))))))

(define (fast-expt-iter b n)
  (define (iter b n result)
    (cond ((= n 0) result)
          ((even? n) (iter (square b) (/ n 2) result))
          (else (iter b (- n 1) (* b result)))))
  (iter b n 1))           