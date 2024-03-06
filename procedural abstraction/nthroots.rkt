#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (recursive n)
    (if (= n 1) 
        f
        (compose (recursive (- n 1)) f)))
  (recursive n))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (repeated-average-damp f n)
  ((repeated average-damp n) f))

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

(define (nth-root x n)
  (fixed-point-tracable (repeated-average-damp (lambda (y) (/ x (expt y (- n 1)))) (floor (log n 2))) 1.0))

(nth-root 2 258)

; 4th root needs 2 average damps, 8th root needs 3, 16th root needs 4 and so on. log n to the base 2 average damps are required for convergence of fixed point