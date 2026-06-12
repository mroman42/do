#lang racket

(require do/monad/norm)
(require do/notation/unbias/leftDo)

;; This file formalizes Newcomb's problem.

(define (newcomb x)
  (do Norm
       (action) <- (uniform '(oneBox) '(twoBox))
       () <- (observe action x)
       (prediction) <- (uniform '(oneBox) '(twoBox))
       () <- (observe action prediction)
       return (action prediction)))
