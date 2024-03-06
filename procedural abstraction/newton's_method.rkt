#lang racket

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x)) 
                 dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method f guess)
  (fixed-point (newton-transform f) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))


(newtons-method (cubic -6 11 -6) 1.0)
(newtons-method (cubic -16 83 -140) 1.0)