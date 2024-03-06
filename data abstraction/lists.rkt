#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))


(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

; (last-pair (list 1 3 4 5))
; (last-pair (list 1))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; (append (list 1 2 3 4) (list 5 6 7 8))

(define (reverse1 lst)
  (if (null? lst)
      lst
      (append (reverse (cdr lst)) (list (car lst)))))

(define (reverse2 lst)
  (define (iter lst result)
    (if (null? lst)
        result
        (iter (cdr lst) (cons (car lst) result))))
  (iter lst '()))

(define (deep-reverse1 lst)
  (cond ((null? lst) lst)
        ((not (pair? (car lst))) (append (deep-reverse1 (cdr lst)) (list (car lst))))
        (else (append (deep-reverse1 (cdr lst)) (list (deep-reverse1 (car lst)))))))

(define (deep-reverse2 lst)
  (define (iter lst result)
    (cond ((null? lst) result)
          ((not (pair? (car lst))) (iter (cdr lst) (cons (car lst) result)))
          (else (iter (cdr lst) (cons (iter (car lst) '()) result)))))
  (iter lst '())
)

(define (map proc items)
  (if (null? items)
    items
    (cons (proc (car items)) (map proc (cdr items)))))

(define (deep-reverse3 lst)
  (if (not (pair? lst))
      lst
      (reverse2 (map deep-reverse3 lst))))

; (deep-reverse3 '((1 2) (3 4)))
; (deep-reverse3 '((1 2 (3 4 5)) 6 (7 8) 9))      

; (reverse1 (list 1 2 3 4))
; (reverse1 (list 1))
; (reverse1 (list))
; (reverse2 (list 1 2 3 4))
; (reverse2 (list 1))
; (reverse2 (list))
; (reverse2 (list (list 1 2) (list 3 4)))

; (define x (list (list 1 2) (list 3 4)))
; x
; (reverse2 x)
; (deep-reverse1 x)
; (deep-reverse2 x)

(define (fringe-v1 lst)
  (cond ((null? lst) lst)
        ((not (pair? lst)) (list lst))
        (else (append (fringe-v1 (car lst)) (fringe-v1 (cdr lst))))))

(define (fringe-v2 lst)
  (cond ((null? lst) lst)
        ((not (pair? (car lst))) (cons (car lst) (fringe-v2 (cdr lst))))
        (else (append (fringe-v2 (car lst)) (fringe-v2 (cdr lst))))))

(define (fringe-v3 lst)
  ; iter succesively reduces the inputted list by cdr ing it
  (define (iter lst f is-outer-list?) 
  ; f keeps record of the leaves using cons and succesive application of itself
  ; list of leaves is formed by applying f to nil when finally original list has been cdr ed into empty.
  ; is-outer-list? ensures nil is applied only once to make final list, is #t for outermost list 
  ; and #f for any sub-lists found within the original list 
    (cond ((null? lst) (if is-outer-list?
                           (f lst)
                           f))
          ; edge case                 
          ((not (pair? lst)) lst)
          ; if first element is a leaf, add it to the function  
          ((not (pair? (car lst)))
             (iter (cdr lst) 
                   (lambda (x) (f (cons (car lst) x))) 
                   is-outer-list?))
          ; otherwise add all the leaves of first element to the function
          (else (iter (cdr lst) 
                      (iter (car lst) f #f) 
                      is-outer-list?))
  ))
  (iter lst (lambda (x) x) #t))

 (define (fringe-iter-reverse items) 
   (define (iter first rest res) 
     (cond ((and (null? first) (null? rest)) res) 
           ((null? first) (iter (car rest) (cdr rest) res)) 
           ((pair? first) (iter (car first) (cons (cdr first) rest) res)) 
           (else (iter (car rest) (cdr rest) (cons first res))))) 
   (iter '() items '())) 

; (fringe-v1 (list 1 (list 1 2) 3 (list 8 9 (list 10 11))))
(fringe-v2 (list 1 (list 1 2) 3 (list 8 9 (list 10 11))))
; (fringe-v3 (list 1 (list 1 2) 3 (list 8 9 (list 10 11))))
; (fringe-iter-reverse (list 1 (list 1 2) 3 (list 8 9 (list 10 11))))