#lang racket

(define (runtime) (current-inexact-milliseconds))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

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

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define empty-board '())

(define (create-move piece-col piece-row)
  (list piece-col piece-row))

(define (get-piece-col-from-move move)
  (car move))  

(define (get-piece-row-from-move move)
  (cadr move))

(define (create-chessboard-config new-move old-config)
  (cons new-move old-config))

(define (get-kth-move k board-config)
  (car (filter (lambda (move) (= (car move) k)) board-config)))

(define (get-last-move config)
  (car config))

(define (adjoin-position new-row k old-config)
  (let ((kth-move (create-move k new-row)))
    (create-chessboard-config kth-move old-config)))

; (safe k board-config) checks if piece moved in the kth move is safe wrt all other pieces moved in all other moves

(define (remove-kth-move k board-config)
  (filter (lambda (move) (not (= (car move) k))) board-config))

(define (remove-last-move config)
  (cdr config))

; checks whether move1 is safe wrt move2
(define (safe-wrt-move? move1 move2)
  (define row-safe?
    (not (= (get-piece-row-from-move move1)
            (get-piece-row-from-move move2))))
  (define col-safe?
    (not (= (get-piece-col-from-move move1)
            (get-piece-col-from-move move2))))
  (define diag-safe?
    (not (= (abs (- (get-piece-row-from-move move1)
                    (get-piece-row-from-move move2)))
            (abs (- (get-piece-col-from-move move1)
                    (get-piece-col-from-move move2))))))
  (and row-safe? col-safe? diag-safe?)) 

; checks whether move is safe wrt all moves of a given board-config 
(define (safe-wrt-config? move config)
  (if (null? config)
      #t
      (and (safe-wrt-move? move (get-last-move config))
           (safe-wrt-config? move (remove-last-move config)))))   

(define (safe? k board-config)
  (let ((kth-move (get-kth-move k board-config))
        (remaining-moves (remove-kth-move k board-config)))
    (if (null? remaining-moves)
        #t
        (safe-wrt-config? kth-move remaining-moves))))

; queens procedure generates a sequence of all possible board configurations that have n queens placed on n x n board without checking each other
(define (queens1 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (configuration) (safe? k configuration))
          (flatmap
            (lambda (old-configuration)
              (map (lambda (new-row)
                     (adjoin-position new-row k old-configuration))
                    (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (queens2 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (configuration) (safe? k configuration))
          (flatmap
            (lambda (new-row)
              (map (lambda (old-configuration)
                     (adjoin-position new-row k old-configuration))
                    (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (timed-queens n start-time)
  (queens2 n)
  (- (runtime) start-time))

(timed-queens 5 (runtime))

; (define move1 (create-move 1 4))
; (newline)
; (display move1)
; (define board-config1 (create-chessboard-config move1 empty-board))
; (newline)
; (display board-config1)
; (define move2 (create-move 2 6))
; (newline)
; (display move2)
; (define board-config2 (create-chessboard-config move2 board-config1))
; (newline)
; (display board-config2)
; (define move3 (create-move 3 2))
; (newline)
; (display move3)
; (define board-config3 (create-chessboard-config move3 board-config2))
; (newline)
; (display board-config3)
; (newline)
; (get-kth-move 3 board-config3)

; (queens 8)

