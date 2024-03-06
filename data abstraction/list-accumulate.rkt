#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; (accumulate + 0 (list 1 2 3 4 5))
; (accumulate * 1 (list 1 2 3 4 5))
; (accumulate cons '() (list 1 2 3 4 5))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (square x)
  (* x x))

; (map square (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

; (append (list 1 2 3 4) (list 5 6 7 8))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; (length (list 1 2 3 4))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; (horner-eval 2 (list 1 3 0 5 0 1))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (sub-tree)
                         (if (not (pair? sub-tree)) 
                             1 
                             (count-leaves sub-tree)))
                       t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
                         (cdr rest))))
  (iter initial sequence))

(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

; (reverse1 (list 1 2 3 4 5))

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

; (reverse2 (list 1 2 3 4 5))

         