#lang racket

(require do/leftdo)
(require do/monad)
(require do/monad/norm)
(require do/example/data-napkin)

;; This file presents an example of M-bias.

;; The following does not recover p, because Y is not independent from Z.
(define (recover-p)
  (lDo Norm
       w <- (lDo Norm
                 (list w z x y) <- p
                 return w)
       z <- (lDo Norm
                 (list w0 z x y) <- p
                 '() <- (observe w w0)
                 return z)
       x <- (lDo Norm
                 (list w0 z0 x y) <- p
                 '() <- (observe w w0)
                 '() <- (observe z z0)
                 return x)
       y <- (lDo Norm
                 (list w0 z0 x0 y) <- p
                 '() <- (observe w w0)
                 ;'() <- (observe z z0)
                 '() <- (observe x x0)
                 return y)
       return (list w z x y)))
