#lang racket

(require do/monad/writer)
(require do/notation/rightDo)

(define Int
  (monoid 0 (lambda (x y) (+ x y))))

(do (Writer Int)
    (x) <- (cons 3 '("x"))
    (y) <- (cons 2 '("y"))
    return (x y))

