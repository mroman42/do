#lang racket

(require do/distributions-ring)
(require do/monad)

(define rationals (ring 0 1 + * zero? /))

()

(define Norm (ring->distMonad rationals))
(define norm-return (monad-return Norm))
(define norm-bind (monad-bind Norm))
(define norm-map (monad-map Norm))
(define norm-join (monad-join Norm))

(provide norm-return norm-bind norm-map norm-join)
