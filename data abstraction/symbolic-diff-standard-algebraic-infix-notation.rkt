#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))            

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence))))) 

(define (sum? expr)
  (eq? '+ (lowest-precedence-operator expr)))

(define (product? expr)
  (eq? '* (lowest-precedence-operator expr)))

(define (lowest-precedence-operator expr)
  (accumulate (lambda (a b)
                (if (operator? a)
                    (min-precedence a b)
                    b))
              'maxop
              expr))

(define *precedence-table*
  '( (maxop . 10000)
     (minop . -10000)
     (+ . 0)
     (* . 1)))

(define (operator? x)
  (define (loop op-pair)
    (cond ((null? op-pair) #f)
           ((eq? x (caar op-pair)) #t)
           (else (loop (cdr op-pair)))))
  (loop *precedence-table*))

(define (min-precedence a b)
  (if (< (precedence a) (precedence b))
      a
      b))

(define (precedence op)
  (define (loop op-pair)
    (cond ((null? op-pair)
           (error "Operator not defined -- PRECEDENCE:" op))
          ((eq? op (caar op-pair))
           (cdar op-pair))
          (else (loop (cdr op-pair)))))
  (loop *precedence-table*))

(define (augend expr)
  (let ((a (cdr (memq '+ expr))))
    (if (singleton? a)
        (car a)
        a)))

(define (singleton? x)
  (and (pair? x) (= (length x) 1)))

(define (prefix sym list)
  (if (or (null? list) (eq? sym (car list)))
      '()
      (cons (car list) (prefix sym (cdr list)))))

(define (addend expr)
  (let ((a (prefix '+ expr)))
    (if (singleton? a)
        (car a)
        a)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (multiplier expr)
  (let ((m (prefix '* expr)))
    (if (singleton? m)
        (car m)
        m)))  

(define (multiplicand expr)
  (let ((m (cdr (memq '* expr))))
    (if (singleton? m)
        (car m)
        m)))   

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))                                                               

(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + 3) 'x)
(deriv '(x * y * (x + 3)) 'x)
(deriv '((x * y) * (x + 3)) 'x)        