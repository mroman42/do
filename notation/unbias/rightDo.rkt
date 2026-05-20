#lang racket

(require do/monad)
(require do/notation/rightDo)

(define-syntax do
  (syntax-rules (<- return)
    [(do m (var ...) <- mexp rest ...)
     ((monad-bind m) mexp (match-lambda [(list var ...) (do m rest ...)]))]
    [(do m return (var ...))
     ((monad-return m) (list var ...))]))

(define-syntax rightDo
  (syntax-rules (<- return)
    [(rightDo i ...) (do i ...)]))

(provide do)
(provide rightDo)
