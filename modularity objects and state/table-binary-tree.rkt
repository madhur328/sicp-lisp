#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

(define (entry tree) (mcar tree))

(define (left-branch tree) (mcar (mcdr tree)))

(define (right-branch tree) (mcar (mcdr (mcdr tree))))

(define (set-left-branch! tree)
  (set-mcar! (mcdr tree)))

(define (set-right-branch! tree)
  (set-mcar! (mcdr (mcdr tree))))

(define (adjoin-record record records)
  (cond ((null? records) (make-tree record '() '()))
        ((= (mcar record) (mcar (entry records))) records)
        ((< (mcar record) (mcar (entry records)))
         (make-tree (entry records)
                    (adjoin-record record (left-branch records))
                    (right-branch records)))
        ((> (mcar record) (mcar (entry records)))
         (make-tree (entry records)
                    (left-branch records)
                    (adjoin-record record (right-branch records))))))

(define (make-tree entry left right)
  (mlist entry left right))


(define (assoc key records)
  (cond ((null? records) #f)
        ((= key (mcar (entry records)))
         (entry records))
        ((< key (mcar (entry records)))
         (assoc key (left-branch records)))
        (else (assoc key (right-branch records)))))

(define (make-table) (mlist '*table*))

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        #f)))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table (adjoin-record (mcons key value) (mcdr table))))))

(define table1 (make-table))
(insert! 5 'value5 table1)                
(insert! 1 'value1 table1)                
(insert! 3 'value3 table1)                
(insert! 9 'value9 table1)                
(insert! 7 'value7 table1)                
(insert! 11 'value11 table1)
(lookup 11 table1)                