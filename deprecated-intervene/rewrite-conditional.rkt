#lang racket

(require do/monad)
(require do/monad/norm)
(require do/leftdo)

(define-syntax Conditional
  (syntax-rules ()
    [(Conditional p (v ...) (y ...)
                  (x ...) (a ...))
     (lDo Norm
          (list v ...) <- p
          '() <- (observe (list x ...) (list a ...))
          return (list y ...))]))

(define-syntax Marginal
  (syntax-rules ()
    [(Marginal p (v ...) (y ...))
     (Conditional p (v ...) (y ...) () ())]))

(provide Conditional)
(provide Marginal)





