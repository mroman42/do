#lang racket

(require do/monad)
(require do/distributions)
(require (except-in do/leftDo do))

(define-syntax do
  (syntax-rules (<- return)
    [(do x ...) (leftDo Norm x ...)]))

(provide do)
(provide (all-from-out do/monad))
(provide (all-from-out do/distributions))

