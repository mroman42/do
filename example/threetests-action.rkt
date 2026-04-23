#lang racket

(require leftdo/leftdo-actdo)
(require leftdo/action-bagdist)

(define test (actDo BagNorm
       x <- (uniform 2 3 2)
       y <~ (bag 'a 'b)
       return (list x y)))

(define (testing patient)
  (match patient
    ['ill      (distribution ['positive 90/100] ['negative 10/100])]
    ['healthy  (distribution ['positive  5/100] ['negative 95/100])]))

(actDo BagNorm
       p <- (distribution ['ill 5/100] ['healthy 95/100])
       o <- (actDo BagNorm
              r <~ (bag 'positive 'positive 'negative)
              t <- (testing p)
              o <- (observe 'positive t)
              return o)
       return p)

