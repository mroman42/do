#lang racket

;; smoking.rkt
;;
;; This file contains an example of causality and intervention analysis.
;; It follows an example of the front-door criterion from the work of Pearl
;; Glymour, and Jewell (page 66). Numbers come from Table 3.1 there.
;;
;; Reference.
;; Causal Inference in Statistics: A Primer -- Pearl, Glymour, and Jewell.

(require do/notation/normDo)
(require do/intervene/derive-interventions)


(define survey
  (distribution-table
     ['(smoker tar nocancer)     323]
     ['(smoker tar cancer)        57]
     ['(nonsmoker tar nocancer)    1]
     ['(nonsmoker tar cancer)     19]
     ['(smoker notar nocancer)    18]
     ['(smoker notar cancer)       2]
     ['(nonsmoker notar nocancer) 38]
     ['(nonsmoker notar cancer)  342]))

;; What would be the incidence if only the 5% of the population were to smoke?

(define (incidence-from-habits habits)
  (Intervene survey
   WithModel (do
      gene <- ()
      smoking <- (gene)
      tar <- (smoking)
      cancer <- (gene tar)
      visibles (smoking tar cancer))
   Setting (smoking) To (smoking-habits)
   In (cancer)))

(do (smoking-habits) <- (distribution
                          ['(smoker)     5/100]
                          ['(nonsmoker) 95/100])
    (incidence) <- (incidence-from-habits smoking-habits)  
    return (incidence))


(define (estimate-intervention i)
  (do 
      (zp) <- (do 
                  (x z y) <- survey
                  () <- (observe i x)
                  return (z))
      (xp) <- (do 
                  (x z y) <- survey
                  return (x))
      (y)  <- (do 
                  (x z y) <- survey
                  () <- (observe x xp)
                  () <- (observe z zp)
                  return (y))
      return (y)))


(Intervene survey
  WithModel (do
    gene <- ()
    smoking <- (gene)
    tar <- (smoking)
    cancer <- (gene tar)
    visibles (smoking tar cancer))
  Setting (smoking) To ('nonsmoker) In (cancer))

;(estimate-intervention 'smoker)
;(estimate-intervention 'nonsmoker)

(provide survey)
