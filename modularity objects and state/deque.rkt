#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; make a d-cell which is essentially a triple consisting of an item, 
; a pointer to prev d-cell and a pointer to next d-cell
(define (make-d-cell item prev-cell-ptr next-cell-ptr)
  (mcons item (mcons prev-cell-ptr next-cell-ptr)))

(define (item d-cell)
  (mcar d-cell))

(define (prev-cell-ptr d-cell)
  (mcar (mcdr d-cell)))

(define (next-cell-ptr d-cell)
  (mcdr (mcdr d-cell)))

(define (set-prev-cell-ptr! d-cell prev-d-cell)
  (set-mcar! (mcdr d-cell) prev-d-cell))  

(define (set-next-cell-ptr! d-cell next-d-cell)
  (set-mcdr! (mcdr d-cell) next-d-cell))  

(define (first-cell? d-cell)
  (null? (prev-cell-ptr d-cell)))  

(define (last-cell? d-cell)
  (null? (next-cell-ptr d-cell)))  

; front pointer points to first d-cell
(define (front-ptr deque)
  (mcar deque))

; rear pointer points to last d-cell
(define (rear-ptr deque)
  (mcdr deque))

(define (set-front-ptr! deque d-cell)
  (set-mcar! deque d-cell))  

(define (set-rear-ptr! deque d-cell)
  (set-mcdr! deque d-cell))  

(define (print-deque deque)
  (define (iter d-cell)
    (cond ((null? d-cell) (newline))
          ((last-cell? d-cell) 
           (begin (display (item d-cell))
                  (newline)))
          (else (begin (display (item d-cell))
                       (iter (next-cell-ptr d-cell))))))
  (iter (front-ptr deque))) 

; CONSTRUCTOR
(define (make-deque)
  (mcons '() '()))

; PREDICATE
(define (empty-deque? deque)
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))      

; SELECTORS
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque")
      (item (front-ptr deque))))  ; get item of the first d-cell

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque")
      (item (rear-ptr deque))))   ; get item of the last d-cell       

; MUTATORS
(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((first-d-cell (make-d-cell item '() '())))  ; for first item, create d-cell with prev and next cell pointers as nil
           (set-front-ptr! deque first-d-cell)  ; and update front and rear pointers to point to that cell
           (set-rear-ptr! deque first-d-cell)
           (print-deque deque)))
         (else (let ((first-d-cell (make-d-cell item '() (front-ptr deque))))  ; otherwise make first d-cell with next cell pointer pointing to the previous first
                 (set-prev-cell-ptr! (front-ptr deque) first-d-cell) ; connect previous cell pointer of the old first to the new first cell
                 (set-front-ptr! deque first-d-cell)  ; and update front pointer of the deque to point to new first d-cell
                 (print-deque deque)))))

(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((last-d-cell (make-d-cell item '() '())))  ; for first item, create d-cell with prev and next cell pointers as nil
           (set-front-ptr! deque last-d-cell)  ; and update front and rear pointers to point to that cell
           (set-rear-ptr! deque last-d-cell)
           (print-deque deque)))
         (else (let ((last-d-cell (make-d-cell item (rear-ptr deque) '())))  ; otherwise make last d-cell with prev cell pointer pointing to old last
                 (set-next-cell-ptr! (rear-ptr deque) last-d-cell); connect next cell pointer of old last to the new last cell
                 (set-rear-ptr! deque last-d-cell)    ; and update rear pointer of the deque to point to new last d-cell
                 (print-deque deque)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque"))
        ((first-cell? (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         (print-deque deque))
        (else
         (set-front-ptr! deque (next-cell-ptr (front-ptr deque)))  ; set deque front pointer to point at second cell
         (set-prev-cell-ptr! (front-ptr deque) '())  ; make that cell's previous cell pointer as nil
         (print-deque deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque) 
         (error "DELETE! called with an empty deque"))
        ((last-cell? (front-ptr deque))
         (set-rear-ptr! deque '())
         (set-front-ptr! deque '())
         (print-deque deque))
        (else
         (set-rear-ptr! deque (prev-cell-ptr (rear-ptr deque)))  ; set deque rear pointer to point at old second last cell
         (set-next-cell-ptr! (rear-ptr deque) '())  ; and set that cell's next cell pointer to nil
         (print-deque deque))))

(define d1 (make-deque))
(print-deque d1)
(front-insert-deque! d1 'b)
(front-insert-deque! d1 'a)
(rear-insert-deque! d1 'c)
; (front-deque d1)
; (rear-deque d1)
(rear-insert-deque! d1 'd)
; d1
(front-delete-deque! d1)
(rear-delete-deque! d1)


(front-delete-deque! d1)
(rear-delete-deque! d1)