#lang racket

(require "magmad.rkt")


;; This is an implementation of Definition 4.12.
(define-syntax rightDo
  (syntax-rules (<- return)

    ;; This is clause (i) in Definition 4.12.
    [
     ;; This is the left-hand side of the equation.
     (rightDo T
              (x1 ...) <- m
              rest ...)
     
     ;; This is the right-hand side of the equation.
     ((magmad-bind T) m
                      (match-lambda [(list x1 ...) (rightDo T
                                                            rest ...)]))
     ]

    ;; This is clause (ii) in Definition 4.12
    [
     ;; This is the left-hand side of the equation.
     (rightDo T
              return (x1 ...))

     ;; This is the right-hand side of the equation.
     ((magmad-return T) (list x1 ...))
     ]))

(provide rightDo)


;; The implementation is repeated twice so that it can be called either as `do`
;; or as `rightDo`. This repetition also provides a compact version without
;; intermediate comments.
(define-syntax do
  (syntax-rules (<- return)
    [(do m (var ...) <- mexp rest ...)
     ((magmad-bind m) mexp (match-lambda [(list var ...) (do m rest ...)]))]
    [(do m return (var ...))
     ((magmad-return m) (list var ...))]))

(provide do)
