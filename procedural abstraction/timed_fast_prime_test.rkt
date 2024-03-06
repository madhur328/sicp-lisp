#lang racket/base

(require "prime.rkt")

(define (runtime) (current-inexact-milliseconds))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) m))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))


(define (even? a)
  (= (remainder a 2) 0))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))
  
(define (square x)
  (* x x))

(define (timed-fast-prime n times)
  (define (inner-func start-time)
    (fast-prime? n times)
    (- (runtime) start-time))
  (inner-func (runtime)))

(define (search-for-primes start end count)
  (define (iter start-time test-prime primes-find result)
    (cond ((or (> test-prime end) (= primes-find count)) result)
          ((prime? test-prime) (iter (runtime) (+ test-prime 2) (+ primes-find 1) (cons (list test-prime (- (runtime) start-time)) result)))
          (else (iter (runtime) (+ test-prime 2) primes-find result))))
  (iter (runtime) (if (even? start) (+ start 1) (+ start 2)) 0 '()))


(search-for-primes 1000 1200 3)
(search-for-primes 10000 10200 3)
(search-for-primes 100000 100200 3)
(search-for-primes 1000000 1000200 3)

; (timed-fast-prime 1009 7)
; (timed-fast-prime 1013 7)
; (timed-fast-prime 1019 7)
; (timed-fast-prime 10007 7)
; (timed-fast-prime 10009 7)
; (timed-fast-prime 10037 7)
; (timed-fast-prime 100003 7)
; (timed-fast-prime 100019 7)
; (timed-fast-prime 100043 7)
; (timed-fast-prime 1000003 7)
; (timed-fast-prime 1000019 7)
; (timed-fast-prime 1000043 7)