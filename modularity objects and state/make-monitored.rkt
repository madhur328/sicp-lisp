#lang racket

(define (make-monitored f)
  (let ((calls 0)) 
    (define (mf arg)
        (cond ((eq? arg 'how-many-calls?) calls)
            ((eq? arg 'reset-count) (set! calls 0) "call count has been reset")
            (else (set! calls (+ calls 1)) (f arg))))
    mf))

(define s (make-monitored sqrt))

(s 100)
(s 'how-many-calls?)
(s 10000)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)