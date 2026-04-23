#lang racket

(require leftdo/monad)
(require (except-in leftdo/monad/norm))
(require leftdo/monad/bag)
(require (except-in leftdo/monad/normbag bag distribution observe))
(require leftdo/action)

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
