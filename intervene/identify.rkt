#lang racket

(require rebellion/collection/multiset)
(require do/intervene/dag)
(require do/monad)
(require do/monad/norm)
(require do/leftdo)

;; ID-ALGORITHM
;; sv: variables being identified.
;; tv: variables being intervened.
;; g : graphical model
;; p : distribution
(define (id-algorithm sv tv g p)

  ;; #1. Ancestors of the identified variables in the intervened graph.
  (define d
    (dag-topological-sort-of 
     (dag-visible-ancestors sv (dag-remove tv g)) g))

  ;; #2. Writes down the line of v.
  ;; (define (line-of v)
  ;;   (define naturals-before-in-component '())
  ;;   (define naturals-until-in-component '())
  ;;   (define identify-until '())
  ;;   (syntax-conditioning (list v)
  ;;                        naturals-before-in-component
  ;;                        naturals-in-component
  ;;                        identify-until))

  d)



(id-algorithm '(y) '(x)
              (Dag
                'u1 <- '()
                'u2 <- '()
                'w <- '(u1 u2)
                'x <- '(w u1)
                'z <- '(x)
                'y <- '(z u2)
                visible '(w x z y))
              'p)
