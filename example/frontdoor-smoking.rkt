#lang racket

;; smoking.rkt
;;
;; This file contains an example of causality and intervention analysis.
;; It follows an example of the front-door criterion from the work of Pearl
;; Glymour, and Jewell (page 66). Numbers come from Table 3.1 there.
;;
;; Reference.
;; Causal Inference in Statistics: A Primer -- Pearl, Glymour, and Jewell.


;(require do/notation/unbias/leftDo)
(require do/monad)
(require do/monad/norm)
(require do/leftdo)
(require do/intervene/rewrite-conditional)

(define survey
  (distribution
     ['(smoker tar nocancer)    323/800]
     ['(smoker tar cancer)       57/800]
     ['(nonsmoker tar nocancer)    1/800]
     ['(nonsmoker tar cancer)     19/800]
     ['(smoker notar nocancer)   18/800]
     ['(smoker notar cancer)      2/800]
     ['(nonsmoker notar nocancer) 38/800]
     ['(nonsmoker notar cancer)  342/800]))


(define (estimate-intervention i)
  (do Norm
      (zp) <- (do Norm
                  (x z y) <- survey
                  () <- (observe i x)
                  return (z))
      (xp) <- (do Norm
                  (x z y) <- survey
                  return (x))
      (y)  <- (do Norm
                  (x z y) <- survey
                  () <- (observe x xp)
                  () <- (observe z zp)
                  return (y))
      return (y)))


(estimate-intervention 'smoker)
(estimate-intervention 'nonsmoker)

