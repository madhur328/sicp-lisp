#lang racket

(define (square x) (* x x))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
(iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (inside-circle-test xl xu yl yu cx cy r)
  (<= (+ (square (- (random-in-range xl xu) cx)) 
         (square (- (random-in-range yl yu) cy))) 
      (square r)))

(define (estimate-pi-experiment)
  (inside-circle-test -1 1 -1 1 0 0 1))  

(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (- x2 x1) (- y2 y1) (exact->inexact (monte-carlo trials P))))

(define (estimate-pi trials)
  (estimate-integral estimate-pi-experiment -1 1 -1 1 trials))

(estimate-pi 1000)    
(estimate-pi 1000000)    

