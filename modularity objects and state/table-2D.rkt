#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((and (number? x) (number? y)) (= x y))
        ((and (symbol? x) (symbol? y)) (eq? x y))
        ((and (mpair? x) (mpair? y)) (and (equal? (mcar x) (mcar y)) 
                                        (equal? (mcdr x) (mcdr y))))
        (else #f)))

(define (make-table)
  (mlist '*table*))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (mcdr table))))
    (if subtable
        (let ((record (assoc key-2 (mcdr subtable))))
          (if record
              (mcdr table)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (mcdr table))))
    (if subtable
        (let ((record (assoc key-2 (mcdr subtable))))
          (if record
              (set-mcdr! record value)
              (set-mcdr! subtable
                         (mcons (mcons key-2 value) 
                                (mcdr subtable)))))
        (set-mcdr! table
                  (mcons (mlist key-1 
                               (mcons key-2 value)) 
                         (mcdr table)))))
  'ok)        
        
                        