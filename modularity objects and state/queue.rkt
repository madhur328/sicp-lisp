#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)
(provide (all-defined-out))

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (make-queue) (mcons '() '()))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-mcdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (mcdr (front-ptr queue)))
          queue)))

; version1
; (define (print-queue queue)
;   (newline)
;   (display (front-ptr queue)))

; version 2
 (define (print-queue q) 
   (define (iter x) 
     (if (null? x) 
         (newline) 
         (begin (display (mcar x)) 
                (iter (mcdr x))))) 
   (iter (front-ptr q))) 

; (define q1 (make-queue))

; (insert-queue! q1 'a)
; (insert-queue! q1 'b)
; (delete-queue! q1)
; (delete-queue! q1)
; (print-queue (insert-queue! q1 'a))
; (print-queue (insert-queue! q1 'b))
; (print-queue (delete-queue! q1))
; (print-queue (delete-queue! q1))

; Ex 3.22 queue as a procedure with local state

; (define (make-queue)
;   (let ((front-ptr '())
;         (rear-ptr '()))
;     (define (print-queue)
;         (define (iter x) 
;             (if (null? x) 
;                 (newline) 
;                 (begin (display (mcar x)) 
;                         (iter (mcdr x))))) 
;         (iter front-ptr))    
;     (define (empty-queue?) (null? front-ptr))
;     (define (front-queue) 
;       (if (empty-queue?)
;           (error "FRONT called with an empty queue" front-ptr)
;           (mcar front-ptr)))
;     (define (insert-queue! item)
;       (let ((new-pair (mcons item '())))
;         (cond ((empty-queue?) 
;                (set! front-ptr new-pair)
;                (set! rear-ptr new-pair)
;                (print-queue))
;               (else
;                (set-mcdr! rear-ptr new-pair)
;                (set! rear-ptr new-pair)
;                (print-queue)))))
;     (define (delete-queue!)
;       (cond ((empty-queue?)
;              (error "DELETE! called with an empty queue" front-ptr))
;             (else
;               (set! front-ptr (mcdr front-ptr))
;               (print-queue))))           
;     (define (dispatch m)
;       (cond ((eq? m 'empty-queue?) empty-queue?)
;             ((eq? m 'front-queue) front-queue)
;             ((eq? m 'insert-queue!) insert-queue!)
;             ((eq? m 'delete-queue!) delete-queue!)))
;     dispatch))

; (define (empty-queue? queue) ((queue 'empty-queue?)))
; (define (front-queue queue) ((queue 'front-queue)))
; (define (insert-queue! queue item) ((queue 'insert-queue!) item))
; (define (delete-queue! queue) ((queue 'delete-queue!)))

; (define q1 (make-queue))
; (insert-queue! q1 'a)
; (insert-queue! q1 'b)
; (delete-queue! q1)
; (delete-queue! q1)    