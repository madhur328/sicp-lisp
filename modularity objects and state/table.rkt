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

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                   (mcons (mcons key value) (mcdr table)))))
  'ok)