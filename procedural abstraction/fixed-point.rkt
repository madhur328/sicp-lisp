#lang racket

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

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)  ;golden ratio
(fixed-point-tracable (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point-tracable (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0) ;damped