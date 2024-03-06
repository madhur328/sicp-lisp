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

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))        

; (define (make-table)
;   (let ((local-table (mlist '*table*)))
;     (define (lookup key-1 key-2)
;       (let ((subtable (assoc key-1 (mcdr local-table))))
;         (if subtable
;             (let ((record (assoc key-2 (mcdr subtable))))
;               (if record
;                   (mcdr record)
;                   #f))
;             #f)))
;     (define (insert! key-1 key-2 value)
;       (let ((subtable (assoc key-1 (mcdr local-table))))
;         (if subtable
;             (let ((record (assoc key-2 (mcdr subtable))))
;               (if record
;                   (set-mcdr! record value)
;                   (set-mcdr! subtable
;                             (mcons (mcons key-2 value)
;                                   (mcdr subtable)))))
;             (set-mcdr! local-table
;                       (mcons (mlist key-1
;                                   (mcons key-2 value))
;                             (mcdr local-table)))))
;       'ok)
;     (define (dispatch m)
;       (cond ((eq? m 'lookup-proc) lookup)
;             ((eq? m 'insert-proc!) insert!)
;             (else (error "Unknown operation -- TABLE" m))))
;     dispatch))        

; (define operation-table (make-table))
; (define get (operation-table 'lookup-proc))
; (define put (operation-table 'insert-proc!))    

; Ex. 3.24
(define (make-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))        

(define operation-table (make-table (lambda (x y) (< (abs (- x y)) 0.1))))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))    

(put 1.0 1.0 'hello) 
(get 1.01 1.01)