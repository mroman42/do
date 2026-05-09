#lang racket

(require do/intervene/syntax)
(require do/precondition)

;; NORMOBSERVATIONS
;; List of observations.
(define (normObservations vs ws next)
  (precondition (equal? (length vs) (length ws))
                'normObservations "observation lists of different length ~v ~v" vs ws)
  
  (match vs
    [(cons v vs)
     (match ws [(cons w ws) (normObservation v w (normObservations vs ws next))])]
    ['() next]))


;(display-program (normObservations '(x y z) '(a b c) (normReturn '(x))))

(provide normObservations)
