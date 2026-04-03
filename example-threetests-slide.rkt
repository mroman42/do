#lang racket

(require leftdo/do-slide)
(require leftdo/slide-normbag)

(slideDo Frequentist
   x <- (uniform 1 2 3)
   y <- (uniform 'a 'b)
   z <~ (bag 'u 'v 'w)
   return (list x z))
