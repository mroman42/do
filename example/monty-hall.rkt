#lang racket

;; MONTY HALL.
;; This file implements the Monty Hall problem, a famous
;; probability puzzle originally posed by Steve Selvin in
;; a letter to American Statistician.
;;
;; Reference:
;;  Letters to the Editor. American Statistician.
;;  Steve Selvin, 1975.

(require (except-in do/notation/unbias/leftDo do))
(require do/notation/unbias/rightDo)
(require do/monad/norm)


;; DESCRIPTION.2

;; We are in a game show, and a prize (a car, in the
;; original is hidden behind one of three doors (left,
;; middle, and right). We constestant picks a door (say,
;; the middle one). For dramatic effect, the host opens
;; one of the non-chosen doors (say, the left one). The
;; host does so avoiding the door that does contain the
;; car (for it would spoil the show) and otherwise
;; randomly and uniformly. Finally, the host offers us to
;; change doors and pick the other one that remains
;; closed. Should we change doors?

;; HOST.
;; Let us first formalize the behaviour of the host: it
;; picks randomly and uniformly among the doors that have
;; not been chosen and that, moreover, do not contain the
;; car.
(define (host car choice)
  (match (cons car choice)
    [(cons 'left   'left)   (uniform '(middle) '(right))]
    [(cons 'left   'middle) (uniform '(right))]
    [(cons 'left   'right)  (uniform '(left))]
    [(cons 'middle 'left)   (uniform '(right))]
    [(cons 'middle 'middle) (uniform '(left) '(right))]
    [(cons 'middle 'right)  (uniform '(left))]
    [(cons 'right  'left)   (uniform '(middle))]
    [(cons 'right  'middle) (uniform '(left))]
    [(cons 'right  'right)  (uniform '(left) '(middle))]))

;; FORMULATION.

;; Let us formalize the Monty Hall problem using
;; do-notation. We repeat the exact same formalization
;; twice: once using right-associating do-notation and
;; once using left-associating do-notation. The result
;; will be different in both cases.
;;
;; The program lines mean that
;; (1) the car is distributed uniformly;
;; (2) the host (knowing our choice) opens a door;
;; (3) we observe the host opened the left door.
;;
;; What is the probability distribution of the car?

(define l-monty-hall
  (leftDo Norm
    (car) <- (uniform '(left) '(middle) '(right))
    (door) <- (host car 'middle)
    () <- (observe door 'left)
    return (car)))

(define r-monty-hall
  (rightDo Norm
    (car) <- (uniform '(left) '(middle) '(right))
    (door) <- (host car 'middle)
    () <- (observe door 'left)
    return (car)))

