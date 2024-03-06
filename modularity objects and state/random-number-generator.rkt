#lang racket

(define random-init 4)

(define (rand-update x) (+ x 1))

; Old rand procedure
; (define rand
;   (let ((x random-init))
;     (lambda ()
;       (set! x (rand-update x))
;       x)))

; Ex. 3.6
(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x)) x)
            ((eq? m 'reset) (lambda (new-value) (set! x new-value) new-value))
            (else (error "Unknown request --RAND" m))))))

(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 1)
(rand 'generate)