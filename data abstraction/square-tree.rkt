#lang racket

(define (square x)
  (* x x))

(define (square-tree1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree) 
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree))) 
       tree))

(define (tree-map proc tree)
  (if (not (pair? tree))
      (proc tree)
      (map (lambda (sub-tree) 
             (tree-map proc sub-tree)) 
           tree)))

(define (square-tree3 tree)
  (tree-map square tree))                

(square-tree1 (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))        

(square-tree2 (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))        

(square-tree3 (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))        