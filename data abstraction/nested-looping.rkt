#lang racket

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (try test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (try (+ test-divisor 1)))))
  (try 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


; (accumulate append
;             '()
;             (map  (lambda (i)
;                     (map (lambda (j) (list i j))
;                          (enumerate-interval 1 (- i 1))))
;                   (enumerate-interval 1 10)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list  i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                  (map (lambda (p) (cons x p))
                       (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (unique-pairs n)
  (flatmap (lambda (i) 
          (map (lambda (j) (list i j))
               (enumerate-interval 1 (- i 1)))) 
       (enumerate-interval 1 n)))
       
(define (make-fixed-sum? s seq)
  (= (+ (car seq) (cadr seq) (caddr seq)) s)) ; more concise is (= (accumulate + 0 seq) s)
  

(define (fixed-sum-triples n s)
  (filter (lambda (seq) 
            (make-fixed-sum? s seq))
          (flatmap (lambda (i) 
                     (flatmap (lambda (j) 
                                (map (lambda (k) (list i j k)) 
                                     (enumerate-interval 1 (- j 1)))) 
                              (enumerate-interval 1 (- i 1)))) 
                   (enumerate-interval 1 n))))


; (fixed-sum-triples 9 15)
 (define (unique-tuples n k) 
     (cond ((< n k) '()) 
           ((= k 0) (list '())) 
           (else (append (unique-tuples (- n 1) k) 
                         (map (lambda (tuple) (cons n tuple)) 
                              (unique-tuples (- n 1) (- k 1))))))) 

(unique-tuples 4 3)                                                         