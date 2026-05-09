#lang racket

(require do/struct-slide)
(require do/monad/norm)
(require do/monad/bag)
(require (only-in do/monad/normbag normbag-bind))

(define (bag-norm-distribute b f)
  (normbag-bind (dist-return b)
                (lambda (x) (dist-map (lambda (y) (bag-return y)) (f x)))))

(define (bag-norm-morph b)
  (dist-uniform (bag->list b)))

(define Frequentist
  (slide
    Norm
    Bag
    bag-norm-distribute
    bag-norm-morph))

(provide
  Frequentist
  uniform
  distribution
  observe
  bag)
