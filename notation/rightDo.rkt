#lang racket

(require do/monad)

;; Right associative do-notation.
(define-syntax rDo
  (syntax-rules (<- return)
    [(rDo m var <- mexp rest ...)
     ((monad-bind m) mexp (match-lambda [var (rDo m rest ...)]))]
    [(rDo m return value)
     ((monad-return m) value)]))

(define-syntax rightDo
  (syntax-rules (<- return)
    [(rDo m var <- mexp rest ...)
     ((monad-bind m) mexp (match-lambda [var (rDo m rest ...)]))]
    [(rDo m return value)
     ((monad-return m) value)]))


(provide rDo rightDo)
