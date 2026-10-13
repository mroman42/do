#lang racket

;; MONTY HALL.
;;
;; This file implements the Monty Hall problem, a famous probability puzzle
;; originally posed by Steve Selvin in a letter to American Statistician.
;;
;; Reference:
;;  Letters to the Editor. American Statistician.
;;  Steve Selvin, 1975.

(require (except-in "rightDo.rkt" do))
(require (except-in "leftDo.rkt" do))
(require "normDo.rkt")


;; DESCRIPTION.
;;
;; We are in a game show, and a prize (a car, in the original) is hidden behind
;; one of three doors (left, middle, and right). The contestant picks a door
;; (say, the middle one). For dramatic effect, the host opens one of the
;; non-chosen doors (say, the left one). The host does so avoiding the door that
;; does contain the car (for it would spoil the show) and otherwise randomly and
;; uniformly. Finally, the host offers us to change doors and pick the other one
;; that remains closed. Should we change doors?

;; PRIZE
;;
;; The prize is behind one of the doors; we assume it is distributed uniformly.
(define uniformDoor (uniform '(l) '(m) '(r)))

;; HOST.
;;
;; Let us formalize the behaviour of the host: it picks randomly and uniformly
;; among the doors that have not been chosen and that, moreover, do not contain
;; the car.
(define (host-given-choice car choice)
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

(define (host car) (host-given-choice car 'm))

;; FORMULATION.
;;
;; Let us formalize the Monty Hall problem using do-notation. We repeat the
;; exact same formalization twice: once using right-associating do-notation and
;; once using left-associating do-notation. The result will be different in both
;; cases.
;;
;; The program lines mean that
;; 
;; (1) the prize is distributed uniformly;
;; (2) the host (knowing our where the prize is) opens a door;
;; (3) we observe the host opened the left door.
;; 
;; (r) What is the probability distribution of the prize?

(define (l-monty-hall)
  (leftDo Norm
    (prize) <- uniformDoor
    (door) <- (host prize)
    () <- (observe 'l door)
    return (prize)))

(define (r-monty-hall)
  (rightDo Norm
    (prize) <- uniformDoor
    (door) <- (host prize)
    () <- (observe 'l door)
    return (prize)))


;; These two pieces of code follow Figure 1.
(define (ml-monty-hall)

  (do (prize door) <- (do
        (prize) <- uniformDoor
        (door) <- (host prize)
        return (prize door))
      () <- (observe 'l door)
      return (prize))

 )
(define (mr-monty-hall)

  (do (prize) <- uniformDoor
      (door) <- (do (door) <- (host prize)
                    () <- (observe 'l door)
                    return (door))
      return (prize))

)


;; DISPLAY.
(displayln "MONTY HALL PROBLEM.\n")
(displayln "Claim A.1 - Figure 1.")
(displayln "The two programs (leftDo/rightDo) for the Monty Hall problem give two different solutions.")
(printf "First solution computes as follows:  ~a~n" (l-monty-hall))
(printf "Second solution computes as follows: ~a~n" (r-monty-hall))
(printf "~n")
(printf "We find the same solutions if we associate manually, as in Figure 1.~n")
(printf "First solution computes as follows:  ~a~n" (ml-monty-hall))
(printf "Second solution computes as follows: ~a~n" (mr-monty-hall))
(printf "~n")


;; STEP-BY-STEP COMPUTATIONS.
;;
;; The rest of the code illustrates the step-by-step computations. These are
;; used on the paper to help the reader follow the computations of the program.

(define (monty-hall-1)
  (do (prize) <- uniformDoor
      return (prize)))

(define (monty-hall-2)
  (do (prize) <- uniformDoor
      return (prize (do (door) <- (host prize)
                        return (door)))))

(define (monty-hall-m1)
  (do (prize) <- uniformDoor
      (door) <- (host prize)
      return (prize door)))

(define (monty-hall-3)
  (do (prize) <- uniformDoor
      (door) <- (host prize)
      return (prize door (do () <- (observe 'l door)
                        return ()))))

(define (monty-hall-m2)
  (do (prize) <- uniformDoor
      (door) <- (host prize)
      () <- (observe 'l door)
      return (prize door)))

(define (monty-hall-r)
  (do (prize) <- uniformDoor
      (door) <- (host prize)
      () <- (observe 'l door)
      return (prize)))



(displayln "Claim A.2 - Section 1.1.")
(displayln "The first solution, computed step-by-step, goes as follows:")
(printf "(i)   ~a~n"  (monty-hall-1))
(printf "(ii)  ~a~n"  (monty-hall-2))
(printf "(μ1)  ~a~n"  (monty-hall-m1))
(printf "(iii) ~a~n"  (monty-hall-3))
(printf "(μ2)  ~a~n"  (monty-hall-m2))
(printf "(π)   ~a~n"  (monty-hall-r))
(displayln "")



(define (r-monty-hall-1)
  (do (prize) <- uniformDoor
      return (prize)))

(define (r-monty-hall-2)
  (do (prize) <- uniformDoor
      return (prize (do (door) <- (host prize)
                        return (door)))))

(define (r-monty-hall-3)
  (do (prize) <- uniformDoor
      return (prize (do (door) <- (host prize)
                         return (door (do () <- (observe 'l door)
                                          return ()))))))

(define (r-monty-hall-m2)
  (do (prize) <- uniformDoor
      return (prize (do (door) <- (host prize)
                         () <- (observe 'l door)
                         return (door)))))

(define (r-monty-hall-m1)
  (do (prize) <- uniformDoor
      (door) <- (do (door) <- (host prize)
                    () <- (observe 'l door)
                    return (door))
      return (prize door)))

(define (r-monty-hall-r)
  (do (prize) <- uniformDoor
      (door) <- (do (door) <- (host prize)
                    () <- (observe 'l door)
                    return (door))
      return (prize)))


(displayln "Claim A.3 - Section 1.1.")
(displayln "The second solution, computed step-by-step, goes as follows:")
(printf "(i)   ~a~n" (r-monty-hall-1))
(printf "    (ii)  ~a~n" (r-monty-hall-2))
(printf "    (iii) ~a~n" (r-monty-hall-3))
(printf "    (μ2)  ~a~n" (r-monty-hall-m2))
(printf "(μ1)  ~a~n" (r-monty-hall-m1))
(printf "(π)   ~a~n" (r-monty-hall-r))
(displayln "")
(displayln "")
