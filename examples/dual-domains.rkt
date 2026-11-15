#lang racket

(require do/monad/duals)
(require do/monad/dual-norm)
(require do/notation/normDualDo)

(define f
  (distribution ['(a) (d 1 -1)]
                ['(b) (d 0 1)]))

(do (x) <- f
    () <- (do (y) <- f
              () <- (observe x y)
              return ())
    return (x))

(do (x) <- (uniform '(a) '(b))
    () <- (do (y) <- f
              () <- (observe x y)
              return ())
    return (x))

