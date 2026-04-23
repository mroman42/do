#lang racket

(require leftdo/do-slide)
(require leftdo/slide-normbag)

(define (test patient)
  (match patient
    ['ill      (distribution ['positive 90/100] ['negative 10/100])]
    ['healthy  (distribution ['positive  5/100] ['negative 95/100])]))

; Pearl
(slideDo Frequentist
     p <- (distribution ['ill 5/100] ['healthy 95/100])
     r <~ (bag 'positive 'positive 'negative)
     t <- (test p)
     '() <- (observe r t)
     return p)

; Jeffrey
(slideDo Frequentist
     r <~ (bag 'positive 'positive 'negative)
     p <- (distribution ['ill 5/100] ['healthy 95/100])
     t <- (test p)
     '() <- (observe r t)
     return p)



(define example1 
(slideDo Frequentist
   x <- (uniform 1 2 3)
   z <~ (bag 'u 'v 'w)
   y <- (uniform 'a 'b)
   return (list x z)))


