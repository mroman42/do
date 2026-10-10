#lang racket

;; smoking.rkt
;;
;; This file contains an example of causality and intervention analysis.
;; It follows an example of the front-door criterion from the work of Pearl
;; Glymour, and Jewell (page 66). Numbers come from Table 3.1 there.
;;
;; Reference.
;; Causal Inference in Statistics: A Primer -- Pearl, Glymour, and Jewell.

(require do/monad/duals)
(require do/notation/normDualDo)
(require do/intervene/derive-interventions-dual)

(define (↗ x) (d x x))
(define (↘ y) (d y (- y)))

;; Data from an observational survey.
;; The data is fake but it is such that it is interesting
;; to ask the conditional versus interventional queries.
(define/table survey
  (['(smoker tar nocancer)      (d 323 0)]
   ['(smoker tar cancer)        (d 57 0)]
   ['(smoker notar nocancer)    (d 18 0)]
   ['(smoker notar cancer)      (d 2 0 )]
   ['(nonsmoker tar nocancer)   (d 1 0)]
   ['(nonsmoker tar cancer)     (d 19 0)]
   ['(nonsmoker notar nocancer) (d 38 0)]
   ['(nonsmoker notar cancer)   (d 342 0)]))


;; given an interventional model
(intervene survey
  withModel (do gene <- ()
                smoking <- (gene)
                tar <- (smoking)
                cancer <- (tar gene)
                return (smoking tar cancer))
  setting (smoking) to ('smoker) in (cancer))

(intervene survey
 withModel (do gene <- ()
               smoking <- (gene)
               tar <- (smoking)
               cancer <- (tar gene)
               return (smoking tar cancer))
 setting (smoking) to ('nonsmoker) in (cancer))

;; differential query
(do
    (h) <- (do (s t c) <- survey
               return (s))
    
    (h-diff) <- (distribution [(list h)     (d 1 0)]
                              ['(smoker)    (d 0 (- 1))]
                              ['(nonsmoker) (d 0 1)])

    (cancer) <- (intervene survey
                 withModel (do
                               gene <- ()
                               smoking <- (gene)
                               tar <- (smoking)
                               cancer <- (tar gene)
                               return (smoking tar cancer))
                 setting (smoking) to (h-diff) in (cancer))

    return (cancer))


(provide survey)
