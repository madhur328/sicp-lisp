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

(define (fermat n a)
  (= (expmod a n n) a))

(define (test-carmichael n)
  (define (try a)
    (if (= a n)
        #t
        (and (fermat n a) (try (+ a 1)))))
  (try 2))


(test-carmichael 561)
(test-carmichael 1105)
(test-carmichael 1729)
(test-carmichael 2465)
(test-carmichael 2821)
(test-carmichael 6601)