#lang racket

(require do/monad)
(require do/normalized)


(define norm-return dist-return)

(define (norm-bind xs f)
  (dist-normalize (dist-bind xs f)))

(define (norm-map f xs)
  (dist-normalize (dist-map f xs)))

(define Norm
  (monad
    norm-return
    norm-bind
    norm-map))

(define norm-join (monad-join Norm))

(define (observe x y)
  (if (equal? x y)
      (uniform '())
      (uniform)))

(provide
 Norm
 norm-return
 norm-bind
 norm-map
 norm-join
 observe
 from-table
 (all-from-out do/normalized))
