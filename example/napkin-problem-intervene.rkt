#lang racket

(require do/intervene/identify)
(require do/monad)
(require do/monad/norm)
(require do/example/data-napkin)

(IdentifySyntax #'(IdentifySyntax '(y)
                 Setting '(x)
                 To '(p)
                 WithModel (do  'u1 <- '()
                                'u2 <- '()
                                'w <- '(u1 u2)
                                'z <- '(w)
                                'x <- '(u1 z)
                                'y <- '(u2 x)
                                return '(w z x y))
                  In 'p))


;; (define (computed-intervention)
;;   (lDo Norm
;;      (list y) <-
;;        (IdentifySyntax (y)
;;                  Setting (x)
;;                  To ('p)
;;                  WithModel (do u1 <- ()
;;                                 u2 <- ()
;;                                 w <- (u1 u2)
;;                                 z <- (w)
;;                                 x <- (u1 z)
;;                                 y <- (u2 x)
;;                                 return (w z x y))
;;                   In p)
;;        return y))




