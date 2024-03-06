#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (average x y)
  (/ (+ x y) 2.0))

(define (midpoint-segment seg)
  (define mid-x (average (x-point (start-segment seg)) 
                         (x-point (end-segment seg))))
  (define mid-y (average (y-point (start-segment seg)) 
                         (y-point (end-segment seg))))
  (make-point mid-x mid-y))

(define (point-distance p1 p2)
  (define (distance x-diff y-diff)
    (define (square x) (* x x))
    (define (square-sum x y)
      (+ (square x) (square y)))
    (sqrt (square-sum x-diff y-diff)))
  (distance (- (x-point p1) (x-point p2))
            (- (y-point p1) (y-point p2))))

(define (segment-length seg)
  (point-distance (start-segment seg)
                  (end-segment seg)))

(define (segment-slope seg)
  (define y-diff (- (y-point (start-segment seg))
                    (y-point (end-segment seg))))
  (define x-diff (- (x-point (start-segment seg))
                    (x-point (end-segment seg))))
  (/ y-diff x-diff))                

; (define A (make-point 1 1))
; (define B (make-point 4 5))
; (point-distance A B)
; (segment-length (make-segment A B))
; (segment-slope (make-segment A B))

; we need width and height as generic operators (to replace width1 and height1) that calculate width and height for any representation
(define (perimeter rect)
  (* 2 (+  (width1 rect)
           (height1 rect))))

(define (area rect)
  (* (width1 rect)
     (height1 rect)))

; First representation
;  p1 .
;     
;  p2 .     . p3 
(define (make-rect1 p1 p2 p3)
  (cons (make-segment p1 p2) (make-segment p2 p3)))

(define (height1 rect)
  (segment-length (car rect)))

(define (width1 rect)
  (segment-length (cdr rect)))

(define A (make-point 1 1))
(define B (make-point 1 6))
(define C (make-point 6 6))
(define Rect (make-rect1 A B C))
(area Rect)
(perimeter Rect)

; Second representation point(x, y), theta, perpandicular side and parallel side

;     \         /
; perp \       /  parallel
;       \     /  
;    (x, y). /)__theta________

(define (make-rect2 pt theta perp paral)
  (cons pt (cons theta (cons perp paral))))

(define (height2 rect)
  (car (cdr (cdr rect))))

(define (width2 rect)
  (cdr (cdr (cdr rect))))