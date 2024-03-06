#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; (dot-product (list 1 2 3) (list 4 5 6))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

; (matrix-*-vector (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list 10 11 12))

(define (transpose mat)
  (accumulate-n cons '() mat))

; (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list (list 10 11 12) (list 13 14 15) (list 16 17 18)))