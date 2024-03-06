#lang racket
(define rec-count 0)
(define iter-count 0)

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  ; (set! rec-count (+ rec-count 1))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


(define (count-change-iterative amount)
  (define (count-iter ways last-incremented half-dollars quarters dimes nickels pennies)
    ; (set! iter-count (+ iter-count 1)) 
    (cond ((> (+ (* 50 half-dollars)
                 (* 25 quarters)
                 (* 10 dimes)
                 (* 5 nickels)
                 pennies) 
              amount) 
           (cond ((eq? last-incremented 'penny) (count-iter (+ ways 1) 'nickel half-dollars quarters dimes (+ nickels 1) 0))
                 ((eq? last-incremented 'nickel) (count-iter ways 'dime half-dollars quarters (+ dimes 1) 0 0)) 
                 ((eq? last-incremented 'dime) (count-iter ways 'quarter half-dollars (+ quarters 1) 0 0 0)) 
                 ((eq? last-incremented 'quarter) (count-iter ways 'half-dollar (+ half-dollars 1) 0 0 0 0))
                 ((eq? last-incremented 'half-dollar) ways) 
           ))
          (else (count-iter ways 'penny half-dollars quarters dimes nickels (+ pennies 1)))))        
  (count-iter 0 'none 0 0 0 0 0))

(count-change 100)
(count-change-iterative 100)
(newline)
(display rec-count)  ; 15499
(newline)
(display iter-count)  ; 8092