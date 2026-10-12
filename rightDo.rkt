#lang racket

(require "magmad.rkt")

(define-syntax do
  (syntax-rules (<- return)
    [(do m (var ...) <- mexp rest ...)
     ((magmad-bind m) mexp (match-lambda [(list var ...) (do m rest ...)]))]
    [(do m return (var ...))
     ((magmad-return m) (list var ...))]))

(define-syntax rightDo
  (syntax-rules (<- return)
    [(rightDo m (var ...) <- mexp rest ...)
     ((magmad-bind m) mexp (match-lambda [(list var ...) (rightDo m rest ...)]))]
    [(rightDo m return (var ...))
     ((magmad-return m) (list var ...))]))

(provide do)
(provide rightDo)
