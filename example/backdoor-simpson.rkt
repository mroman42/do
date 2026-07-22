#lang racket

(require do/monad)
(require do/monad/norm)
(require do/leftdo)

; Simpson's paradox example.
; (https://www.tedposton.org/Courses/IDMD401S25L10.pdf)
(define survey
  (distribution
     ['(female control attack)    1/120]
     ['(female control benign)   19/120]
     ['(female treatment attack)  3/120]
     ['(female treatment benign) 37/120]
     ['(male control attack)     12/120]
     ['(male control benign)     28/120]
     ['(male treatment attack)    8/120]
     ['(male treatment benign)   12/120]))

(define (estimate-intervention a)
  (do Norm
      (u x1 y1) <- survey
      (y) <- (do Norm
                 (u0 x0 y0) <- survey
                 () <- (observe u0 u)
                 () <- (observe x0 a)
                 return (y0))
      return (y)))

(estimate-intervention 'control)
(estimate-intervention 'treatment)
