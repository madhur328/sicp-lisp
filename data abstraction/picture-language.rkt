#lang racket
(require sicp-pict)
;(paint einstein)

(define wave 
  (segments->painter (list 
                      (segment (vect .25 0) (vect .35 .5)) 
                      (segment (vect .35 .5) (vect .3 .6)) 
                      (segment (vect .3 .6) (vect .15 .4)) 
                      (segment (vect .15 .4) (vect 0 .65)) 
                      (segment (vect 0 .85) (vect .15 .6)) 
                      (segment (vect .15 .6) (vect .3 .65)) 
                      (segment (vect .3 .65) (vect .4 .65)) 
                      (segment (vect .4 .65) (vect .35 .85)) 
                      (segment (vect .35 .85) (vect .4 1)) 
                      (segment (vect .6 1) (vect .65 .85)) 
                      (segment (vect .65 .85) (vect .6 .65)) 
                      (segment (vect .6 .65) (vect .75 .65)) 
                      (segment (vect .75 .65) (vect 1 .35)) 
                      (segment (vect 1 .15) (vect .6 .45)) 
                      (segment (vect .6 .45) (vect .75 0)) 
                      (segment (vect .6 0) (vect .5 .3)) 
                      (segment (vect .5 .3) (vect .4 0)) 
                      )))

; paint wave as four figures as shown in book
(paint wave) 
(paint (transform-painter wave (make-vect 0.0 0.0) (make-vect 0.5 0.0) (make-vect 0.0 1.0)))
(paint (transform-painter wave (make-vect 0.0 0.0) (make-vect 1.0 0.0) (make-vect 0.0 0.5)))
(paint (transform-painter wave (make-vect 0.0 0.0) (make-vect 0.65 0.35) (make-vect 0.35 0.65)))                     

(define wave2 (beside wave (flip-vert wave))) 

(define wave4 (below wave2 wave2))

(define (flipped-pairs painter) 
  (let ((painter2 (beside painter (flip-vert painter)))) 
    (below painter2 painter2)))

(define wave4a (flipped-pairs wave))

(define (right-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (right-split painter (- n 1)))) 
        (beside painter (below smaller smaller)))))

(define (up-split painter n) 
  (if (= n 0) 
      painter 
      (let ((smaller (up-split painter (- n 1)))) 
        (below painter (beside smaller smaller)))))

(define (corner-split painter n) 
  (if (= n 0) 
      painter 
      (let ((up (up-split painter (- n 1))) 
            (right (right-split painter (- n 1)))) 
        (let ((top-left (beside up up)) 
              (bottom-right (below right right)) 
              (corner (corner-split painter (- n 1)))) 
          (beside (below painter top-left) 
                  (below bottom-right corner))))))


(define (square-limit painter n) 
  (let ((quarter (corner-split painter n))) 
    (let ((half (beside (flip-horiz quarter) quarter))) 
      (below (flip-vert half) half))))

;(paint (right-split einstein 3))
;(paint (up-split einstein 3))
;(paint (corner-split einstein 2))
;(paint (square-limit einstein 3))
;(paint (square-limit wave 4))

(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 smaller smaller))))))

(define right-split2 (split beside below))
(define up-split2 (split below beside))

;(paint (right-split2 einstein 3))
;(paint (up-split2 einstein 3))

(define outline
  (segments->painter
   (list
    (segment (vect 0.0 0.0) (vect 0.0 1.0))
    (segment (vect 0.0 0.0) (vect 1.0 0.0))
    (segment (vect 0.0 1.0) (vect 1.0 1.0))
    (segment (vect 1.0 0.0) (vect 1.0 1.0)))))

;(paint outline)

(define x-painter 
  (segments->painter 
   (list 
    (segment (vect 0.0 0.0) (vect 1.0 1.0)) 
    (segment (vect 0.0 1.0) (vect 1.0 0.0)))))

;(paint x-painter)

(define diamond 
  (segments->painter 
   (list 
    (segment (vect 0.0 0.5) (vect 0.5 1.0)) 
    (segment (vect 0.5 1.0) (vect 1.0 0.5)) 
    (segment (vect 1.0 0.5) (vect 0.5 0.0)) 
    (segment (vect 0.5 0.0) (vect 0.0 0.5)))))

;(paint diamond)

(paint wave)
