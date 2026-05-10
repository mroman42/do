#lang racket

(require rebellion/collection/multiset)
(require do/intervene/dag)
(require do/monad)
(require do/monad/norm)
(require do/leftdo)
(require do/intervene/syntax-identify-algorithm)
(require do/intervene/syntax)

;; ID-ALGORITHM
;; sv: variables being identified.
;; tv: variables being intervened.
;; g : graphical model
;; p : distribution
(define (id-algorithm sv tv g p)

  ;; #1. Ancestors of the identified variables in the intervened graph.
  (define d
    (dag-topological-sort-of ;; topologically sorted 
     (dag-visible-ancestors  ;; ancestors
      sv                     ;; of the identified variables
      (dag-remove tv g))     ;; in the intervened graph
     g))

  ;; #2. Writes down the line of v in D.
  (define (line-of v)
    (define ws (dag-c-component-of v (dag-restricted (dag-visible-ancestors sv (dag-remove tv g)) g)))
    (define us (dag-c-component-of v g))
    ; note this is not the component!
    (define qs (sce us (filter (lambda (x) (not (member x us))) (dag-visibles g)) g p))
    (define idpart (identify-algorithm ws us qs g))
    idpart)

  (line-of 'y))
  
  
  ;;   (define naturals-before-in-component '())
  ;;   (define naturals-until-in-component '())
  ;;   (define identify-until '())
  ;;   (syntax-conditioning (list v)
  ;;                        naturals-before-in-component
  ;;                        naturals-in-component
  ;;                        identify-until))





;; (normProgram
;;  (id-algorithm '(y) '(x)
;;               (Dag
;;                 'u1 <- '()
;;                 'u2 <- '()
;;                 'w <- '(u1 u2)
;;                 'x <- '(w u1)
;;                 'z <- '(x)
;;                 'y <- '(z u2)
;;                 visible '(w x z y))
;;               (normProgram 'q)))

(normProgram
 (id-algorithm '(y) '(x)
              (Dag
                'u1 <- '()
                'u2 <- '()
                'w <- '(u1 u2)
                'z <- '(w)
                'x <- '(z u1)
                'y <- '(z u2)
                visible '(w x z y))
              (normProgram 'q)))
