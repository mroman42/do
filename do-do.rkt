#lang racket

(require leftdo/monad)

(define-syntax do
  (syntax-rules (<- return)

    [ (do m
          return value)
     
      ((monad-return m) value) ]

    [ (do m
          var <- mexp
          rest ...)
     
     ((monad-bind m) mexp (match-lambda [var (do m rest ...)])) ]))

(provide do)

