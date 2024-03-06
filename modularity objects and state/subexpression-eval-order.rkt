#lang racket

(define f
  (let ((state 0))
    (lambda (value) 
      (if (and (= state 1)
               (= value 0))
          1
          (begin (set! state value) 0)))))

; will return 0 if arguments of + are evaluated from left to right,
; will return 1 if arguments of + are evaluated from right to left
(+ (f 0) (f 1))                 
             