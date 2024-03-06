#lang racket

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

; Strategy1 generic operations with explicit dispatch: To add a new type, we create a new representation
; with all the constructors and selectors and for each generic operation, we add a conditional clause
; including the new type and new procedure to be applied for the data type. So adding a new type needs a lot
; of code modification in all the old generic operations to include that type. While adding a new operation
; doesn't need to modify any old code as we have to write conditional clauses for each existing type to include
; the new operation. In short, adding new operation is easy, adding new type is hard

; Strategy2 data-directed style: To add new type, we create new installation package involving constructors
; and selectors for the type along with interface for adding all existing operations to table of op-type indexed entries.
; to add a new operation, we add the operation to all the packages representing existing types.
; Adding new type adds more columns to the table, and adding an operation adds more rows to the table, both of which
; can be done without modifying old code. In short both adding type and adding operation is easy.

; Strategy3 message-passing style: To add a new type, new data object is created with all existing operations that can be
; dispatched. To add a new operation we need to modify all existing data types to include the new operation. In short,
; adding new type is easy, while adding new operation is hard. 