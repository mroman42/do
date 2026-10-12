#lang racket

(require "magmad.rkt")
(require "distributions.rkt")
(require (except-in "leftDo.rkt" do))

(define-syntax do
  (syntax-rules (<- return)

    ;; This rule implements Remark 4.15. We use the name Norm, the paper uses
    ;; the name MD for the normalized distribution magmad.
    [(do x ...) (leftDo Norm x ...)]

    ))

(provide do)
(provide (all-from-out "magmad.rkt"))
(provide (all-from-out "distributions.rkt"))

