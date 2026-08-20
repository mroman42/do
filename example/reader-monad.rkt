#lang racket

(require do/monad/reader)
(require do/notation/rightDo)

((do Reader
    (x) <- (lambda (s) (list (+ 1 s)))
    (y) <- (lambda (s) (list (* 2 x s)))
    return (x y))
 3)
