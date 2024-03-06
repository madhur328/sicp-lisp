#lang racket

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))

(define (append lst1 lst2)
  (accumulate cons lst2 lst1))           

(define (equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((and (number? x) (number? y)) (= x y))
        ((and (symbol? x) (symbol? y)) (eq? x y))
        ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) 
                                        (equal? (cdr x) (cdr y))))
        (else #f)))

(define empty-set '())

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; procedure is same if duplicate elements are allowed in set        

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; if duplicate elements are allowed

(define (adjoin-set1 x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; procedure is same if duplicate elements are allowed in set        

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

(define (union-set1 set1 set2)
  (append set1 set2))                      