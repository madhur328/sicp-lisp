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

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))

(define (cube x)
  (* x x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (inc n) (+ n 1))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpson-next x)
    (+ x h))
  (define (simpson-term x)
    (cond ((or (= x a) (= x b)) (f x))
          ((even? (/ (- x a) h)) (* 2 (f x)))
          (else (* 4 (f x)))))
  (* (sum simpson-term a simpson-next b) (/ h 3)))

(define (simpson-integral-v2 f a b n) 
  (define h (/ (- b a) n)) 
  (define (yk k) (f (+ a (* h k)))) 
  (define (simpson-term k) 
    (* (cond ((or (= k 0) (= k n)) 1) 
             ((odd? k) 4) 
             (else 2)) 
       (yk k))) 
  (* (/ h 3) (sum simpson-term 0 inc n)))  

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)
(simpson-integral-v2 cube 0 1.0 100) ;v2 doesn't throw error for non-integer a , b
(simpson-integral-v2 cube 0 1.0 1000)