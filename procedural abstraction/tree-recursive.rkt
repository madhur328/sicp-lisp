#lang racket

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))
       

(define (f-iter n)
  (define (iter a b c counter)
    (if (= counter n)
        a
        (iter b 
              c 
              (+ c (* 2 b) (* 3 a)) 
              (+ counter 1))))
  (iter 0 1 2 0))

(define (pascal-elem r c)
  (if (or (= c 1) (= r c))
      1
      (+ (pascal-elem (- r 1) c)
         (pascal-elem (- r 1) (- c 1)))))
       
       