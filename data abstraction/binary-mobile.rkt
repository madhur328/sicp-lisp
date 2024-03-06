#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mob)
  (car mob))

(define (right-branch mob)
  (car (cdr mob)))

(define (branch-length br)
  (car br))

(define (branch-structure br)
  (car (cdr br)))

(define (branch-weight br)
  (let ((structure (branch-structure br)))
    (if (not (pair? structure))
      structure
      (+ (branch-weight (left-branch structure))
         (branch-weight (right-branch structure))))))

(define (total-weight mob)
  (+ (branch-weight (left-branch mob))
     (branch-weight (right-branch mob))))

(define (balanced? mob)
  (let ((lb (left-branch mob))
        (rb (right-branch mob)))
    (and (= (* (branch-length lb)
               (branch-weight lb))
            (* (branch-length rb)
               (branch-weight rb)))
         (if (not (pair? (branch-structure lb)))
             #t
             (balanced? (branch-structure lb)))
         (if (not (pair? (branch-structure rb)))
             #t
             (balanced? (branch-structure rb))))))

(define b1 (make-branch 3 3))
(define b2 (make-branch 5 5))
(define mob1 (make-mobile b1 b2))
(define b3 (make-branch 4 4))
(define b4 (make-branch 6 mob1))
(define mob2 (make-mobile b3 b4))
(define b5 (make-branch 6 3))
(define b6 (make-branch 3 6))
(define mob3 (make-mobile b5 b6))
(define b7 (make-branch 9 2))
(define b8 (make-branch 2 mob3))
(define mob4 (make-mobile b7 b8))

(branch-weight b1)
(branch-weight b2)
(branch-weight b3)
(branch-weight b4)
(total-weight mob1)
(total-weight mob2)
(balanced? mob1)
(balanced? mob2)
(balanced? mob3)
(balanced? mob4)