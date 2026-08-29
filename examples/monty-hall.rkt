#lang racket

;; MONTY HALL.
;; This file implements the Monty Hall problem, a famous
;; probability puzzle originally posed by Steve Selvin in
;; a letter to American Statistician.
;;
;; Reference:
;;  Letters to the Editor. American Statistician.
;;  Steve Selvin, 1975.

(require (except-in do/notation/leftDo do))
(require do/notation/rightDo)
(require do/monad/norm)


;; DESCRIPTION.

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

(define uniformDoor (uniform '(L) '(M) '(R)))

;; HOST.
;; Let us first formalize the behaviour of the host: it
;; picks randomly and uniformly among the doors that have
;; not been chosen and that, moreover, do not contain the
;; car.
(define (host car choice)
  (match (cons car choice)
    [(cons 'L 'L)  (uniform '(M) '(R))]
    [(cons 'L 'M)  (uniform '(R))]
    [(cons 'L 'R)  (uniform '(L))]
    [(cons 'M 'L)  (uniform '(R))]
    [(cons 'M 'M)  (uniform '(L) '(R))]
    [(cons 'M 'R)  (uniform '(L))]
    [(cons 'R 'L)  (uniform '(M))]
    [(cons 'R 'M)  (uniform '(L))]
    [(cons 'R 'R)  (uniform '(L) '(M))]))

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

(define (l-monty-hall)
  (leftDo Norm
    (prize) <- uniformDoor
    (door) <- (host prize 'M)
    () <- (observe door 'L)
    return (prize)))

(define (r-monty-hall)
  (rightDo Norm
    (prize) <- uniformDoor
    (door) <- (host prize 'M)
    () <- (observe door 'L)
    return (prize)))

(l-monty-hall)
(r-monty-hall)

