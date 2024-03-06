#lang racket

(define (scale-tree1 tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree1 (car tree) factor)
                    (scale-tree1 (cdr tree) factor)))))


(define (map proc items)
  (if (null? items)
    items
    (cons (proc (car items)) (map proc (cdr items)))))


(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree2 sub-tree factor)
             (* sub-tree factor)))
       tree))            


(scale-tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

(scale-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)