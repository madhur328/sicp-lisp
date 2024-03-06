#lang racket

(define *op-table* (make-hash))

(define (put op type item)
  (hash-set! *op-table* (list op type) item))

(define (get op type)
  (hash-ref *op-table* (list op type) #f))

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))  

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types --APPLY-GENERIC"
            (list op type-tags))))))   

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))            
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; deriv function using data directed style

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))        

(define (install-sum-package)
  ;; internal procedures
  (define (addend args) (car args))
  (define (augend args) (cadr args))
  (define (sum-deriv args var)
    (make-sum (deriv (addend args) var)
              (deriv (augend args) var)))               
  ;; interface
  (put 'deriv '+ sum-deriv)      
  'done)

(define (install-product-package)
  ;;internal procedures
  (define (multiplier args) (car args))
  (define (multiplicand args) (cadr args))
  (define (prod-deriv args var)
    (make-sum (make-product (deriv (multiplier args) var)
                            (multiplicand args))
              (make-product (multiplier args)
                            (deriv (multiplicand args) var))))
  ;; interface
  (put 'deriv '* prod-deriv)
  'done)

(define (make-exponentiation b e)
  (cond ((=number? b 1) 1)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        ((not (number? e)) (error "Exponent must be a number in make exponent form --make-exponentiation" e))
        (else (list '** b e))))

(define (install-exponentiation-package)
  ;;internal procedures
  (define (base args) (car args))
  (define (exponent args) (cadr args))
  (define (expo-deriv args var)
    (make-product
      (make-product (exponent args)
                    (make-exponentiation
                      (base args)
                      (- (exponent args) 1)))
      (deriv (base args) var)))
  ;; interface
  (put 'deriv '** expo-deriv)
  )

(install-sum-package)
(install-product-package)
(install-exponentiation-package)
(deriv '(+ (* x y) (* 3 (* x x))) 'x)   

; division files df1 df2 df3 ...
; each division file as ordered list or binary tree or any other representation tagged with division name
; division1 ordered list ('div1 . ( record1 record2 record3 )) | lookup_fn: key, records |-> record 
; division1 record:  (key . (salary . address)) with employee name as key with get_salary selector
; division2 binary tree  (record1 (record2 () ()) (record3 (record4 () ()) ())) | lookup_fn: key, records |-> record
; division2 record:  (key . (address . salary)) with employee name as key with get_salary selector


;a. Implement get-record
;   Each division file must be tagged with division 'type' and
;   provide a instalation package to include specific get-record. 
;   The output is a tagged record. 
;   Get-record must return false if doesn't find employee record (c).
(define (division file) (car file))

(define (records file) (cdr file))

(define (get-record employee-id file)
  (let ((record ((get 'get-record (division file)) employee-id (records file))))
    (if record
       (attach-tag (division file) record)
       #f)))

;b. get-salary 
(define (get-salary record) 
  (let ((division (car record)) 
        (record-content (cdr record))) 
          ((get 'get-salary division) record-content)))

;c. find-employee-record 
(define (find-employee-record employee-id file-list) 
  (if (null? file-list) 
      #f 
      (let ((current-file (car file-list))) 
      (if (get-record employee-id current-file) 
          (get-record employee-id current-file) 
          (find-employee-record employee-id (cdr file-list))))))

;d. New company must tag their record-file with name as in a new division and add 
;   the new division get-record and get-salary implementations to the op-type table.                             



