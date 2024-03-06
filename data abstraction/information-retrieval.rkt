#lang racket

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((and (number? x) (number? y)) (= x y))
        ((and (symbol? x) (symbol? y)) (eq? x y))
        ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) 
                                        (equal? (cdr x) (cdr y))))
        (else #f)))


; set-of-records ( record1 record2 record3 ... )
; record : (key . data)
(define (key record) (car record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define sample-db1 '((1 . data1) (2 . data2) (3 . data3) (4 . data4)))
(lookup 3 sample-db1)

; set of records as binary tree
; (record2 (record1 () ()) (record3 () (record4 () ())))
; record : (key . data)
(define sample-db2 '((2 . data2) ((1 . data1) () ()) ((3 . data3) () ((4 . data4) () ()))))

(define (lookup2 given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup2 given-key (left-branch set-of-records)))
        (else (lookup2 given-key (right-branch set-of-records)))))

(lookup2 3 sample-db2)        
