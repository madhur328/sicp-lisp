#lang racket

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame1 frame)
  (car frame))

(define (edge1-frame1 frame)
  (cadr frame))

(define (edge2-frame1 frame)
  (caddr frame))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (cadr frame))

(define (edge2-frame2 frame)
  (cddr frame))

(define (make-vect x y)
  (cons x y))

(define (x-cor-vect v)
  (car v))

(define (y-cor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (x-cor-vect v1) 
                (x-cor-vect v2))
             (+ (y-cor-vect v1) 
                (y-cor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (x-cor-vect v1) 
                (x-cor-vect v2))
             (- (y-cor-vect v1) 
                (y-cor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (x-cor-vect v))
             (* s (y-cor-vect v))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame1 frame)
      (add-vect (scale-vect (x-cor-vect v)
                            (edge1-frame1 frame))
                (scale-vect (y-cor-vect v)
                            (edge2-frame1 frame))))))