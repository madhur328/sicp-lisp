#lang racket

(define (square x)
  (* x x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
         next
         (try next))))
  (try first-guess))

(define (fixed-point-tracable f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess step)
    (display step)
    (display ".) trying with guess ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
         next
         (try next (+ step 1)))))
  (try first-guess 1))

(define (cont-frac n d k)
  (define (try i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (try (+ i 1))))))
  (try 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           12) ; 1 / (golden ratio)
(+ 2 (cont-frac-iter (lambda (i) 1.0)
                     (lambda (i) (if (= (remainder i 3) 2)
                                     (* 2 (+ (quotient i 3) 1))
                                     1))
                     20))  ; e

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1)
                                  x
                                  (- (square x))))
                  (lambda (i) (- (* 2.0 i) 1))
                  k))

(tan-cf 4 10)