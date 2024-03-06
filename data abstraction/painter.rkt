#lang racket

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))


(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

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
      (origin-frame frame)
      (add-vect (scale-vect (x-cor-vect v)
                            (edge1-frame frame))
                (scale-vect (y-cor-vect v)
                            (edge2-frame frame))))))

(define (for-each proc lst)
  (cond ((null? lst) #t)
        (else (proc (car lst)) (for-each proc (cdr lst)))))    

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))                                

; (define (segments->painter segment-list)
;   (lambda (frame)
;     (for-each
;       (lambda (segment)
;         (draw-line
;           ((frame-coord-map frame) (start-segment segment))
;           ((frame-coord-map frame) (end-segment segment))))
;       segment-list)))

; (define v1 (make-vect 0 0))
; (define v2 (make-vect 0 1))
; (define v3 (make-vect 1 1))
; (define v4 (make-vect 1 0))
; (define v5 (make-vect 0 0.5))
; (define v6 (make-vect 0.5 1))
; (define v7 (make-vect 1 0.5))
; (define v8 (make-vect 0.5 0))
; (define seg1 (make-segment v1 v2))      
; (define seg2 (make-segment v2 v3))      
; (define seg3 (make-segment v3 v4))      
; (define seg4 (make-segment v4 v1))
; (define seg5 (make-segment v1 v3))
; (define seg6 (make-segment v2 v4))
; (define seg7 (make-segment v5 v6))
; (define seg8 (make-segment v6 v7))
; (define seg9 (make-segment v7 v8))
; (define seg10 (make-segment v8 v5))
; (define frame-outline-painter (segments->painter (list seg1 seg2 seg3 seg4)))      
; (define X-painter (segments->painter (list seg5 seg6)))      
; (define diamond-painter (segments->painter (list seg7 seg8 seg9 seg10)))
