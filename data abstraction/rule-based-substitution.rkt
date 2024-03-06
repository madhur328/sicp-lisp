#lang racket

(define deriv-rules
  '(
    ( (dd (?c c) (? v))                 0)
    ( (dd (?v v) (? v))                 1)
    ( (dd (?v u) (? v))                 0)
    
    ( (dd (+ (? x1) (? x2)) (? v))
      (+ (dd (: x1) (: v))
         (dd (: x2) (: v)))             )

    ( (dd (* (? x1) (? x2)) (? v))
      (+ (* (: x1) (dd (: x2) (: v)))
         (* (dd (: x1) (: v)) (: x2)))  )    

    ( (dd (** (? x) (?c n)) (? v))
      (* (* (: n)
            (** (: x) (: (- n 1))))
         (dd (: x) (: v)))              )
  ))

(define algebra-rules
  '(
    ( ((? op) (?c e1) (?c e2))
      (: (op e1 e2))                                )

    ( ((? op) (? e1) (?c e2))
      ((: op) (: e2) (: e1))                        )

    ( (+ 0 (? e))             (: e)                 )

    ( (* 1 (? e))             (: e)                 )

    ( (* 0 (? e))             0                     )

    ( (* (?c e1) (* (?c e2) (? e3)))
      (* (: (* e1 e2)) (: e3))                      )

    ( (* (? e1) (* (?c e2) (? e3)))
      (* (: e2) (* (: e1) (: e3)))                  )

    ( (* (* (? e1) (? e2)) (? e3))
      (* (: e1) (* (: e2) (: e3)))                  )

    ( (+ (?c e1) (+ (?c e2) (? e3)))
      (+ (: (+ e1 e2)) (: e3))                      )

    ( (+ (? e1) (+ (?c e2) (? e3)))
      (+ (: e2) (+ (: e1) (: e3)))                  )

    ( (+ (+ (? e1) (? e2)) (? e3))
      (+ (: e1) (+ (: e2) (: e3)))                  )

    ( (+ (* (?c c) (? a)) (* (?c d) (? a)))
      (* (: (+ c d)) (: a))                         )

    ( (* (? c) (+ (? d) (? e)))
      (+ (* (: c) (: d)) (* (: c) (: e)))           )                    
  ))  

(define Dsimp
  (simplifier deriv-rules))

(Dsimp '(dd (+ x y) x))

(define (atom? x)
  (not (pair? x)))

(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         (if (atom? exp)
             (if (eq? pat exp)
                 dict
                 'failed)
             'failed))
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dict pat exp dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dict pat exp dict))
        ((atom? exp) 'failed)
        (else
         (match (cdr pat)
                (cdr exp)
                (match (car pat)
                       (car exp)
                       dict)))))

(define (arbitrary-constant? pat)
  (and (pair? pat) (eq? (car pat) '?c)))

(define (constant? exp) (number? exp)) 

(define (arbitrary-variable? pat)
  (and (pair? pat) (eq? (car pat) '?v)))

(define (variable? exp)
  (symbol? exp))

(define (arbitrary-expression? pat)
  (and (pair? pat) (eq? (car pat) '?)))    

(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
        (eval (lookup (car form) dict)
              user-initial-environment)
        (mapcar (lambda (v)
                  (lookup v dict))
                (cdr form)))))

(define (skeleton-evaluation? s)
  (and (pair? s) (eq? (car s) ':)))

(define (eval-exp s)
  (cadr s))                 

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp (car exp))
              (simplify-parts (cdr exp)))))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict (match (pattern (car rules))
                             exp
                             (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                  (instantiate
                    (skeleton (car rules))
                    dict))))))
    (scan the-rules))
  simplify-exp)

(define (empty-dictionary) '())

(define (extend-dict pat dat dic)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((not v)
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v) var (cadr v))))            