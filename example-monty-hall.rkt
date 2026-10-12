#lang racket

;; MONTY HALL.
;; This file implements the Monty Hall problem, a famous
;; probability puzzle originally posed by Steve Selvin in
;; a letter to American Statistician.
;;
;; Reference:
;;  Letters to the Editor. American Statistician.
;;  Steve Selvin, 1975.

(require "distributions.rkt")
(require (except-in "rightDo.rkt" do))
(require (except-in "leftDo.rkt" do))
(require "normDo.rkt")


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

(define uniformDoor (uniform '(l) '(m) '(r)))


;; HOST.
;; Let us first formalize the behaviour of the host: it
;; picks randomly and uniformly among the doors that have
;; not been chosen and that, moreover, do not contain the
;; car.
(define (host car choice)
  (match (cons car choice)
    [(cons 'l 'l)  (uniform '(m) '(r))]
    [(cons 'l 'm)  (uniform '(r))]
    [(cons 'l 'r)  (uniform '(l))]
    [(cons 'm 'l)  (uniform '(r))]
    [(cons 'm 'm)  (uniform '(l) '(r))]
    [(cons 'm 'r)  (uniform '(l))]
    [(cons 'r 'l)  (uniform '(m))]
    [(cons 'r 'm)  (uniform '(l))]
    [(cons 'r 'r)  (uniform '(l) '(m))]))


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
    (door) <- (host prize 'm)
    () <- (observe door 'l)
    return (prize)))

(define (r-monty-hall)
  (rightDo Norm
    (prize) <- uniformDoor
    (door) <- (host prize 'm)
    () <- (observe door 'l)
    return (prize)))


;; DISPLAY.
(displayln "MONTY HALL PROBLEM.\n")
(displayln "Claim A - Figure 1.")
(displayln "The two programs for the Monty Hall problem give different solutions.")
(printf "First solution computes as follows:  ~a~n" (l-monty-hall))
(printf "Second solution computes as follows: ~a~n" (r-monty-hall))
(display "\n")


;; STEP-BY-STEP COMPUTATIONS.
(define (monty-hall-1)
  (do (prize) <- uniformDoor
      return (prize)))

(define (monty-hall-2)
  (do (prize) <- uniformDoor
      (door) <- (host prize 'm)      
      return (prize door)))

(define (monty-hall-3)
  (do (prize) <- uniformDoor
      (door) <- (host prize 'm)
      () <- (observe door 'l)
      return (prize door)))

(define (monty-hall-r)
  (do (prize) <- uniformDoor
      (door) <- (host prize 'm)
      () <- (observe door 'l)
      return (prize)))

(displayln "Claim B - Section 1.1 BINDING STYLE")
(displayln "The first solution, computed step-by-step:")
(printf "(i)   ~a~n"  (monty-hall-1))
(printf "(ii)  ~a~n"  (monty-hall-2))
(printf "(iii) ~a~n"  (monty-hall-3))
(printf "(r)   ~a~n"  (monty-hall-r))
(displayln "")

(define (e-monty-hall-2)
  (do (prize) <- uniformDoor
      return (prize (do (door) <- (host prize 'm)
                        return (door)))))

(define (e-monty-hall-3)
  (do (prize) <- uniformDoor
      (door) <- (host prize 'm)
      return (prize door (do () <- (observe door 'l)
                        return ()))))


(displayln "Claim B - Section 1.1 MULTIPLICATIVE STYLE")
(displayln "The first solution, computed step-by-step:")
(printf "(i)   ~a~n"  (monty-hall-1))
(printf "(ii)  ~a~n"  (e-monty-hall-2))
(printf "(μ1)  ~a~n"  (monty-hall-2))
(printf "(iii) ~a~n"  (e-monty-hall-3))
(printf "(μ2)  ~a~n"  (monty-hall-3))
(printf "(π)   ~a~n"  (monty-hall-r))
(displayln "")



(define (r-monty-hall-1)
  (do (prize) <- uniformDoor
      return (prize)))

(define (r-monty-hall-2)
  (do (prize) <- uniformDoor
      return (prize (do (door) <- (host prize 'm)
                        return (door)))))

(define (r-monty-hall-3)
  (do (prize) <- uniformDoor
      return (prize (do (door) <- (host prize 'm)
                         () <- (observe door 'l)
                         return (door)))))

(define (r-monty-hall-r1)
  (do (prize) <- uniformDoor
      (door) <- (do (door) <- (host prize 'm)
                    () <- (observe door 'l)
                    return (door))
      return (prize door)))

(define (r-monty-hall-r2)
  (do (prize) <- uniformDoor
      (door) <- (do (door) <- (host prize 'm)
                    () <- (observe door 'l)
                    return (door))
      return (prize)))

(displayln "Claim C - Section 1.1 BINDING STYLE")
(displayln "The second solution, computed step-by-step:")
(printf "(i)   ~a~n" (r-monty-hall-1))
(printf "    (ii)  ~a~n" (r-monty-hall-2))
(printf "    (iii) ~a~n" (r-monty-hall-3))
(printf "    (r1)  ~a~n" (r-monty-hall-r1))
(printf "(r2)  ~a~n" (r-monty-hall-r2))
(displayln "")
;(displayln "")

(define (a-monty-hall-3)
  (do (prize) <- uniformDoor
      return (prize (do (door) <- (host prize 'm)
                         return (door (do () <- (observe door 'l)
                                          return ()))))))

(displayln "Claim C - Section 1.1 MULTIPLICATIVE STYLE")
(displayln "The second solution, computed step-by-step:")
(printf "(i)   ~a~n" (r-monty-hall-1))
(printf "    (ii)  ~a~n" (r-monty-hall-2))
(printf "    (iii) ~a~n" (a-monty-hall-3))
(printf "    (n1)  ~a~n" (r-monty-hall-3))
(printf "(n2)  ~a~n" (r-monty-hall-r1))
(printf "(r)   ~a~n" (r-monty-hall-r2))
(displayln "")
(displayln "")
