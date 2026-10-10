#lang racket

(require "monad.rkt")
(require "distributions.rkt")
(require (except-in "leftDo.rkt" do))

(define-syntax do
  (syntax-rules (<- return)
    [(do x ...) (leftDo Norm x ...)]))

(provide do)
(provide (all-from-out "monad.rkt"))
(provide (all-from-out "distributions.rkt"))

