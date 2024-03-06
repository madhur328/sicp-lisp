#lang racket

(define (square x)
  (* x x))

(define (even? a)
  (= (remainder a 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) m))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))

(define (mod-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) 
          (remainder-square-checked (mod-expmod base (/ exp 2) m) m))
        (else
          (remainder (* base (mod-expmod base (- exp 1) m)) m))))

(define (remainder-square-checked a m)
  (if (and (not (or (= a 1) (= a (- m 1))))
           (= (remainder (square a) m) 1))
      0
      (remainder (square a) m)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (mod-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 2 10)
(fast-prime? 1009 10)
(fast-prime? 4 10)
(fast-prime? 99 10)
(fast-prime? 561 10)