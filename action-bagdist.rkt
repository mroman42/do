#lang racket

(require do/monad)
(require (except-in do/monad/norm))
(require do/monad/bag)
(require (except-in do/monad/normbag bag distribution observe))
(require do/action)

(define (bag-norm-act b f)
  (frequentist 
   (normbag-bind (dist-return b)
                (lambda (x) (dist-map (lambda (x) (bag-return x)) (f x))))))

(define BagNorm
  (action
    Norm
    bag-norm-act))

(provide BagNorm
         uniform
         bag
         distribution
         observe)
