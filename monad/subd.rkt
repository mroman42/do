#lang racket

(require leftdo/monad)
(require leftdo/normalized)

(define Subd
  (monad dist-return dist-bind dist-map))

(define (observe x y)
  (if (equal? x y)
      (uniform '())
      (uniform)))


(provide Subd
         distribution
         uniform
         observe)
