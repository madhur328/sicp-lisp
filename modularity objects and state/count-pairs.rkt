#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

; returns number of pairs (doesn't care if the pairs are distinct)
(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs (mcar x))
         (count-pairs (mcdr x))
         1)))


; mmemq like memq but for mutable lists
(define (count-distinct-pairs x) 
   (let ((encountered '())) 
     (define (helper x) 
       (if (or (not (mpair? x)) (mmemq x encountered)) 
         0 
         (begin 
           (set! encountered (mcons x encountered)) 
           (+ (helper (mcar x)) 
              (helper (mcdr x)) 
              1)))) 
   (helper x)))               

; different structures made of 3 distinct pairs
(define x1 (mcons (mcons 1 2) (mcons 3 4)))  ; 3 pairs
(count-pairs x1)
(count-distinct-pairs x1)
(define x21 (mcons 2 3))
(define x2 (mcons (mcons 1 x21) x21)) ; 4 pairs
(count-pairs x2)
(count-distinct-pairs x2)
(define x31 (mcons 2 3))
(define x32 (mcons x31 x31))
(define x3 (mcons x32 x32))  ; 7 pairs
(count-pairs x3)
(count-distinct-pairs x3)
(define x4 (mlist 1 2 3))
(set-mcdr! (last-pair x4) x4) ; infinite pairs
; (count-pairs x4)
(count-distinct-pairs x4)

(define (has-cycle? x)
  (let ((encountered '()))
    (define (helper x)
      (cond ((not (mpair? x)) #f)
            ((mmemq x encountered) #t)
            (else 
              (set! encountered (mcons x encountered)) 
              (helper (mcdr x)))))
    (helper x)))


; based on floyd's cycle detection algorithm
; Note that the space is constant because the process is iterative and the parameters and a and b are pointers 
; -- (cdr lst) is a pointer to, not a separate copy of the rest of the lst.
 (define (cycle-const-space? lst) 
   (define (safe-mcdr l) 
     (if (mpair? l) 
         (mcdr l) 
         '())) 
   (define (iter a b) 
     (cond ((not (mpair? a)) #f) 
           ((not (mpair? b)) #f) 
           ((eq? a b) #t) 
          ;  ((eq? a (safe-mcdr b)) #t) 
           (else (iter (safe-mcdr a) (safe-mcdr (safe-mcdr b)))))) 
   (iter (safe-mcdr lst) (safe-mcdr (safe-mcdr lst)))) 



(has-cycle? x1) 
(cycle-const-space? x1)   
(has-cycle? x2)   
(cycle-const-space? x2)   
(has-cycle? x3) 
(cycle-const-space? x3)     
(has-cycle? x4)    
(cycle-const-space? x4) 

(define x5 (mlist 1 2 3 4))
(set-mcdr! (last-pair x5) x5)
(cycle-const-space? x5) 

(define x6 (mlist 1 2 3 4 5))
(set-mcdr! (last-pair x6) x6)
(cycle-const-space? x6) 

(define x7 (mlist 1 2 3 4 5 6))
(set-mcdr! (last-pair x7) x7)
(cycle-const-space? x7) 