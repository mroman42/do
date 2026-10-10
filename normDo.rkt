#lang racket

(require do/monad)
(require do/monad/norm)
(require (except-in do/notation/leftDo do))

(define-syntax do
  (syntax-rules (<- return)
    [(do x ...) (leftDo Norm x ...)]))

(provide do)
(provide (all-from-out do/monad))
(provide (all-from-out do/monad/norm))

