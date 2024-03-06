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

(define (lookup key-list table)
 (define (iter key-list table)
   (if (null? key-list)
       (mcdr table)
       (let ((subtable (assoc (car key-list) (mcdr table))))
         (if subtable
             (iter (cdr key-list) subtable)   
             #f))))
 (iter key-list table))

(define (insert! key-list value table)
  (define (iter key-list table)
    (if (null? key-list)
        (set-mcdr! table value)
        (let ((subtable (assoc (car key-list) (mcdr table))))
          (if subtable
              (iter (cdr key-list) subtable)
              (set-mcdr! table (mcons (helper key-list) (mcdr table)))))))
  (define (helper key-list)
    (cond ((null? key-list) value)
          ((= (length key-list) 1) (mcons (car key-list) (helper (cdr key-list))))
          (else (mlist (car key-list) (helper (cdr key-list))))))
  (iter key-list table))

(define table1 (make-table))
(insert! '(key1 key2 key3 key4) '4-key-value table1)                 
(insert! '(key5 key6) '2-key-value table1)
(lookup '(key1 key2 key3 key4) table1)
(lookup '(key5 key6) table1)                   