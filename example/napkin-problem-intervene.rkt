#lang racket

(require do/intervene/identify)
(require do/monad)
(require do/monad/norm)
(require do/example/data-napkin)


(define computed-intervention
  (lDo Norm
     (list w z x y) <-
       (Intervene p
                  WithModel (do u1 <- ()
                                u2 <- ()
                                w <- (u1 u2)
                                z <- (w)
                                x <- (u1 z)
                                y <- (u2 z)
                                return (w z x y))
                  Setting (x) To ('p))
       return y))




