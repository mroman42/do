#lang racket

(require do/intervene/syntax)


(define (normObservations vs ws next)  
  (match vs
    [(cons v vs)
     (match ws [(cons w ws) (normObservation v w (normObservations vs ws next))])]
    ['() next]))


; EXAMPLES
(define example
  (normProgram
   (normObservations
    '(x y z)
    '(a b c)
    (normReturn '(x)))))

(provide normObservations)
