#lang racket

(require do/intervene/dag)

(define-syntax DagSyntax
  (syntax-rules (<-)
    [(Dag o <- is more ...)  (dagDependency #'o #'is (Dag more ...))]
    [(Dag visible xs)        (dagVisible #'xs)]))


(Dag
   u1 <- (list)
   u2 <- (list)
   w <- (list u1 u2)
   z <- (list w)
   x <- (list z u1)
   y <- (list x u2)
   visible (list w z x y))
