#lang racket

; Ex 3.3
; (define (make-account balance account-password)
;   (define (withdraw amount)
;     (if (>= balance amount)
;         (begin (set! balance (- balance amount))
;                balance)
;         "Insufficient funds"))
;   (define (deposit amount)
;     (set! balance (+ balance amount))
;     balance)
;   (define (dispatch password m)
;     (cond ((not (eq? password account-password)) (lambda (x) "Incorrect passowrd"))
;           ((eq? m 'withdraw) withdraw)
;           ((eq? m 'deposit) deposit)
;           (else (error "Unknown request --MAKE-ACCOUNT"
;                        m))))
;   dispatch)


; (define acc (make-account 100 'secret-password))
; ((acc 'secret-password 'withdraw) 40)
; ((acc 'some-other-passowrd 'deposit) 50)


;Ex 3.4
; (define (call-the-cops . arg)
;   "The cops have been called")

; (define (make-account balance account-password)
;   (let ((consecutive-auth-fails 0)
;         (limit 7))
;     (define (withdraw amount)
;         (if (>= balance amount)
;             (begin (set! balance (- balance amount))
;                 balance)
;             "Insufficient funds"))
;     (define (deposit amount)
;         (set! balance (+ balance amount))
;         balance)
;     (define (dispatch password m)
;         (cond ((not (eq? password account-password)) 
;                 (set! consecutive-auth-fails 
;                       (+ consecutive-auth-fails 1)) 
;                 (if (>= consecutive-auth-fails limit) 
;                     call-the-cops 
;                     (lambda (x) "Incorrect passoword")))
;               (else  
;                 (set! consecutive-auth-fails 0) 
;                 (cond ((eq? m 'withdraw) withdraw)
;                       ((eq? m 'deposit) deposit)
;                       (else (error "Unknown request --MAKE-ACCOUNT"
;                             m))))))
;     dispatch))

; (define acc (make-account 100 'secret-password))
; ((acc 'secret-password 'withdraw) 40)
; ((acc 'some-other-passowrd 'deposit) 50)
; ((acc 'some-other-passowrd 'deposit) 50)
; ((acc 'some-other-passowrd 'deposit) 50)
; ((acc 'some-other-passowrd 'deposit) 50)
; ((acc 'some-other-passowrd 'deposit) 50)
; ((acc 'some-other-passowrd 'deposit) 50)
; ((acc 'some-other-passowrd 'deposit) 50)


; Ex 3.7
; (define (make-account balance account-password)
;   (let ((consecutive-auth-fails 0)
;         (limit 7)
;         (password-list (list account-password)))
;     (define (withdraw amount)
;         (if (>= balance amount)
;             (begin (set! balance (- balance amount))
;                 balance)
;             "Insufficient funds"))
;     (define (deposit amount)
;         (set! balance (+ balance amount))
;         balance)
;     (define (dispatch password m)
;         (cond ((not (memq password password-list)) 
;                 (set! consecutive-auth-fails 
;                       (+ consecutive-auth-fails 1)) 
;                 (if (>= consecutive-auth-fails limit) 
;                     call-the-cops 
;                     (lambda (x) "Incorrect passoword")))
;               (else  
;                 (set! consecutive-auth-fails 0) 
;                 (cond ((eq? m 'withdraw) withdraw)
;                       ((eq? m 'deposit) deposit)
;                       ((eq? m 'add-user) (lambda (new-password) (set! password-list (cons new-password password-list))))
;                       (else (error "Unknown request --MAKE-ACCOUNT"
;                             m))))))
;     dispatch))

; (define (make-joint acc password new-password)
;   ((acc password 'add-user) new-password)
;   acc)    


; (define peter-acc (make-account 100 'open-sesame))
; ((peter-acc 'open-sesame 'withdraw) 40)
; ((peter-acc 'open-sesame 'deposit) 60)
; (define paul-acc
;   (make-joint peter-acc 'open-sesame 'rosebud))
; ((paul-acc 'rosebud 'withdraw) 40)  
; ((peter-acc 'rosebud 'withdraw) 10)


; better solution by x3v that keeps different passwords for different users
 (define (make-account balance password) 
   (define incorrect-count 0) 
   (define (withdraw amount) 
     (if (>= balance amount) 
         (begin (set! balance (- balance amount)) 
                balance) 
         "insufficient balance")) 
   (define (deposit amount) 
     (set! balance (+ balance amount)) 
     balance) 
   (define (issue-warning) 
     (if (> incorrect-count 7) 
         (error "the cops are on their way") 
         (error "Wrong password" (- 7 incorrect-count) 'more 'attempts))) 
   (define (auth-layer pw . m) 
     (cond ((null? m) (eq? pw password)) 
           ((eq? pw password) (dispatch (car m))) 
           (else (begin (set! incorrect-count (+ incorrect-count 1)) 
                        (issue-warning))))) 
   (define (dispatch m) 
     (set! incorrect-count 0) 
     (cond ((eq? m 'withdraw) withdraw) 
           ((eq? m 'deposit) deposit) 
           (else (error "Unknown request" m)))) 
   auth-layer) 
  
 (define (make-joint acc pw-prev pw-next) 
   (define (dispatch pw . m) 
     (if (null? m) 
         (eq? pw pw-next) 
         (acc (if (eq? pw pw-next) pw-prev pw-next) (car m)))) 
   (if (acc pw-prev) 
       dispatch 
       (error "Incorrect password to original account" pw-prev))) 
  
 (define peter-acc (make-account 100 'open-sesame)) 
 (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud)) 
 (define pan-acc (make-joint paul-acc 'rosebud 'vvv)) 
  
 ;; tests 
 ((pan-acc 'vvv 'deposit) 100)  ;; 200 
 ((peter-acc 'open-sesame 'deposit) 100) ;; 300 
 ((paul-acc 'rosebud 'deposit) 100) ;; 400 
 ((peter-acc 'rosebud 'deposit) 100)  ;; error as intended 


