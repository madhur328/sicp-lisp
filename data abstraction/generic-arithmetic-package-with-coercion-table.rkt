#lang racket

(define *op-table* (make-hash))

(define (put op type item)
  (hash-set! *op-table* (list op type) item))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define *coercion-table* (make-hash))  

(define (put-coercion type1 type2 item)
  (hash-set! *coercion-table* (list type1 type2) item))

(define (get-coercion type1 type2)
  (hash-ref *coercion-table* (list type1 type2) #f))

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))  

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))       
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))       
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
        (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (arg) (= arg 0.0)))     
'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) ( car x))
  (define (denom x) ( cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))                                                                
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (n1 n2) (= (* (numer n1) (denom n2))
                          (* (numer n2) (denom n1)))))
  (put '=zero? '(rational)
       (lambda (arg) (= (numer arg) 0.0)))                                                                                                      
'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures 
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;;internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))                   
  'done)
   
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))            
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))  

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))  
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))  
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

    ;; interface to rest of the system
    (define (tag z) (attach-tag 'complex z))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))                     
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))                     
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))                     
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))                     
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))                     
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'equ? '(complex complex)
         (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
                              (= (imag-part z1) (imag-part z2)))))
    (put '=zero? '(complex)
         (lambda (arg) (= (magnitude arg) 0.0)))                                                        
  'done)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (if (= (length args) 2)
;               (let ((type1 (car type-tags))
;                     (type2 (cadr type-tags))
;                     (a1 (car args))
;                     (a2 (cadr args)))
;                 (if (eq? type1 type2)
;                     (error "No method for these types"
;                            (list op type-tags))
;                     (let ((t1->t2 (get-coercion type1 type2))
;                       (t2->t1 (get-coercion type2 type1)))
;                   (cond (t1->t2
;                           (apply-generic op (t1->t2 a1) a2))
;                         (t2->t1
;                           (apply-generic op a1 (t2->t1 a2)))
;                         (else
;                           (error "No method for these types"
;                                  (list op type-tags)))))))
;               (error "No method for these types"
;                    (list op type-tags)))))))

(define (any-false? lst)
  (cond ((null? lst) #f)
        ((car lst) (any-false? (cdr lst)))
        (else #t)))          

; gets first feasible coercion where all args have been coerced into same type
; tries first to coerce all args into type of first arg, then second arg and so on
; returns a list of arguments of same type, or #f if all coercions fail
(define (get-feasible-coercion args type-tags)
  (define (iter tags)
    (if (null? tags)
        #f
        (let ((type-to (car tags)))
          (let ((coercions (map (lambda (type-from) 
                                  (if (eq? type-from type-to)
                                      (lambda (x) x)
                                      (get-coercion type-from type-to)))
                                type-tags)))
          (if (any-false? coercions)
              (iter (cdr tags))
              (map (lambda (coercion arg) (coercion arg)) 
                   coercions 
                   args))))))
  (iter type-tags))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coerced-args (get-feasible-coercion args type-tags)))
            (if coerced-args
                (let ((new-type-tags (map type-tag coerced-args)))
                  (let ((new-proc (get op new-type-tags)))
                    (if new-proc
                        (apply new-proc (map contents coerced-args))
                        (error "No method for these types"
                               (list op type-tags)))))
                (error "No method for these types"
                       (list op type-tags))))))))

; Situation where this is not sufficiently general: 
; types: A B C D
; registered ops: op (type-A type-B type-B) or op (type-D type-D type-D) 
; registered coercions: A->B C->B A->D B->D C->D 
; Situation: Evaluating (apply-generic op A B C) will only try (op A B C), (op A A A), (op B B B), (op C C C) and fail  
; while we can just coerce C to B to evaluate (op A B B) instead or coerce all to D                       

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (equ? n1 n2)
  (if (eq? (type-tag n1) (type-tag n2))
      (apply-generic 'equ? n1 n2)
      #f))

(define (=zero? arg)
  (apply-generic '=zero? arg))        

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)


