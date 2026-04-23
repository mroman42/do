#lang racket

(require leftdo/monad)

(define Id
  (monad
   (λ (x) x)
   (λ (xs f) (f xs))
   (λ (f xs) (f xs))))

(provide Id)
