#lang racket

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (identity x) x)

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

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (filtered-accumulate predicate combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((predicate a) (combiner (term a) (filtered-accumulate predicate combiner null-value term (next a) next b)))
        (else (filtered-accumulate predicate combiner null-value term (next a) next b))))

(define (filtered-accumulate-iter predicate combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))


(define (prime-sq-sum a b)
  (filtered-accumulate prime? + 0 square a inc b))

(define (relative-prime-product n)
  (define (relative-prime-predicate a)
    (= (gcd n a) 1))
  (filtered-accumulate relative-prime-predicate * 1 identity 1 inc n))  ; n is always relatively non-prime with itself

(prime-sq-sum 2 7)
(relative-prime-product 5)


