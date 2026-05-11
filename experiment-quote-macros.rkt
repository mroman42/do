#lang racket


(require do/monad/norm)
(require do/leftdo)

(define-syntax (Macro stx)
  (syntax-case stx ()
    [(Identify (quote qx) )
       #`(lDo Norm
              qx <- (uniform 2 3 4)
              return qx)]))

(Macro 'x)


(define-syntax (Macro2 stx)
  (syntax-case stx ()
    [(Identify (list (quote qs) ...))
       #`(lDo Norm
              (list qs ...) <- (uniform (list 2 2) (list 3 4) (list 4 5))
              return (list qs ...))]))

(Macro2 (list 'x 'y))
