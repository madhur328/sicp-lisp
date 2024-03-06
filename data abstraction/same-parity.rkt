#lang racket

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse lst)
  (if (null? lst)
      lst
      (append (reverse (cdr lst)) (list (car lst)))))

(define (odd? x)
  (= (remainder x 2) 1))

(define (even? x)
  (= (remainder x 2) 0))

(define (same-parity x . y)
  (define (iter f lst result)
    (cond ((null? lst) result)
          ((f (car lst)) (iter f (cdr lst) (append result (list (car lst)))))
          (else (iter f (cdr lst) result))))
  (cond ((null? y) (list x))
        ((odd? x) (cons x (iter odd? y (list))))
        (else (cons x (iter even? y (list))))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity 2 4 5 12 6 17 7)

;Using append as above is expensive, as it will iterate through all the elements 
;of the list you build, at _each_ iteration.
;It's better simply to build the list in reverse order, and then reverse the final list at the end



(define (same-parity-v2 first . rest) 
   (let ((yes? (if (even? first) 
                   even? 
                   odd?))) 
     (define (iter items result) 
       (if (null? items) 
           (reverse result) 
           (iter (cdr items) (if (yes? (car items)) 
                                 (cons (car items) result) 
                                 result)))) 
     (iter rest (list first))))

(define (same-parity-v3 x . y)
  (define (same-parity? a)
    (= (remainder a 2) (remainder x 2)))
  (define (iter lst f)
    (if (null? lst) 
        (f '())
        (iter (cdr lst) (if (same-parity? (car lst))
                            (lambda (z) (f (cons (car lst) z)))
                            f))))
  (iter y (lambda (z) (cons x z))))     

(same-parity 1 2 3 4 5 6 7)
(same-parity-v2 2 3 4 5 6 7)
(same-parity-v3 2 4 5 12 6 17 7) 

