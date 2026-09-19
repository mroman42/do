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

;; Data from an observational survey.
;; The data is fake but it is such that it is interesting
;; to ask the conditional versus interventional queries.
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

;; conditioning on smoking, probability of cancer
(do (s t c) <- survey
    () <- (observe s 'smoker)
    return (c))

(do (s t c) <- survey
    () <- (observe s 'nonsmoker)
    return (c))

;; given an interventional model
(intervene survey
 withModel (do
               gene <- ()
               smoking <- (gene)
               tar <- (smoking)
               cancer <- (tar gene)
               return (smoking tar cancer))
 setting (smoking) to ('smoker) in (cancer))

(interveneStx survey
 withModel (do
               gene <- ()
               smoking <- (gene)
               tar <- (smoking)
               cancer <- (tar gene)
               return (smoking tar cancer))
 setting (smoking) to ('nonsmoker) in (cancer))






;; ;; What is your expected incidence of a person that smokes?
;; (do (s t c) <- survey
;;     () <- (observe s 'smoker)
;;     return (c))

;; (do (s t c) <- survey
;;     () <- (observe s 'nonsmoker)
;;     return (c))

;; ;; What is the expected incidence if we make a random person smoke?
;; (intervene survey
;;  withModel (do
;;               gene <- ()
;;               smoking <- (gene)
;;               tar <- (smoking)
;;               cancer <- (gene tar)
;;               return (smoking tar cancer))
;;  setting (smoking) to ('smoker) in (cancer))

;; (interveneStx survey
;;  withModel (do
;;               gene <- ()
;;               smoking <- (gene)
;;               tar <- (smoking)
;;               cancer <- (gene tar)
;;               return (smoking tar cancer))
;;  setting (smoking) to ('nonsmoker) in (cancer))





;; What would be the incidence if only the 5% of the population were to smoke?

;; (define (incidence-from-habits habits)
;;   (intervene survey
;;    withModel (do
;;       gene <- ()
;;       smoking <- (gene)
;;       tar <- (smoking)
;;       cancer <- (gene tar)
;;       visibles (smoking tar cancer))
;;    setting (smoking) to (habits)
;;    in (cancer)))

;; (do (habits) <- (distribution
;;                           ['(smoker)     5/100]
;;                           ['(nonsmoker) 95/100])
;;     (incidence) <- (incidence-from-habits habits)  
;;     return (incidence))


(provide survey)
