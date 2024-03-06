#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; part a
(define (make-semaphore n) 
   (let ((lock (make-mutex)) 
         (taken 0)) 
     (define (semaphore command) 
       (cond ((eq? command 'acquire) 
              (lock 'acquire) 
              (if (< taken n) 
                  (begin (set! taken (+ taken 1)) (lock 'release)) 
                  (begin (lock 'release) (semaphore 'acquire)))) 
             ((eq? command 'release) 
              (lock 'acquire) 
              (set! taken (- taken 1)) 
              (lock 'release)))) 
     semaphore))

; part b

 (define (make-semaphore2 maximum-clients) 
   (let ((access-mutex (list false)) 
         (clients 0)) 
     (define (the-semaphore message) 
       (cond ((eq? message 'acquire) 
              (if (test-and-set! access-mutex) 
                  (the-semaphore 'acquire)
                  'access-mutex-acquired) 
              (cond ((> clients maximum-clients) 
                     (clear! access-mutex) 
                     (the-semaphore 'acquire)) 
                    (else 
                     (set! clients (+ clients 1)) 
                     (clear! access-mutex)))) 
             ((eq? message 'release) 
              (if (test-and-set! access-mutex) 
                  (the-semaphore 'release)
                  'access-mutex-acquired) 
              (set! clients (- clients 1)) 
              (clear! access-mutex)))) 
     the-semaphore)) 

(define (make-mutex)
  (let ((cell (mlist #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)   ; retry
                 'mutex-acquired))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell #f))

(define (test-and-set! cell)
  (if (mcar cell)
      #t
     (begin (set-mcar! cell #t)
            #f)))           