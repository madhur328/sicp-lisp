#lang racket

(define (map proc items)
  (if (null? items)
    items
    (cons (proc (car items)) (map proc (cdr items)))))

(define square
  (lambda (x) (* x x)))

(define (square-list1 items)
  (if (null? items)
      items
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

; (square-list1 (list 1 2 3 4))
; (square-list2 (list 1 2 3 4))

(define (for-each proc lst)
  (cond ((null? lst) #t)
        (else (proc (car lst)) (for-each proc (cdr lst)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
