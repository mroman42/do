#lang racket

;; smoking.rkt
;;
;; This file contains an example of causality and intervention analysis.
;; It follows an example of the front-door criterion from the work of Pearl
;; Glymour, and Jewell (page 66). Numbers come from Table 3.1 there.
;;
;; Reference.
;; Causal Inference in Statistics: A Primer -- Pearl, Glymour, and Jewell.

(require "normDo.rkt")


(define/table P
  (['(smoker tar nocancer)     323]
   ['(smoker tar cancer)        57]
   ['(nonsmoker tar nocancer)    1]
   ['(nonsmoker tar cancer)     19]
   ['(smoker notar nocancer)    18]
   ['(smoker notar cancer)       2]
   ['(nonsmoker notar nocancer) 38]
   ['(nonsmoker notar cancer)  342]))

;; The following code computes the solution to Study Question 4.3.4 from "Causal
;; Inference in Statistics: A Primer" from the survey data. The solution is
;; printed to be 0.5025 at Page 68 of the solution manual ("Causal Inference in
;; Statistics: A Primer - Solution Manual").
;;
;; This code obtains instead the following distribution:
;;   <199/400[nocancer] + 201/400[cancer]>.
;; Note that 199/400 = 0.5025.
;;

(define (smoking-solution)
  (define a 'nonsmoker)
  
  ;; The following is the formula from Theorem 5.17.
  (do 
      (s t0 c0) <- P
      (t) <- (do (s1 t c1) <- P
                 () <- (observe s1 a)
                 return (t))
      (c) <- (do (s2 t2 c) <- P
                 () <- (observe s2 s)
                 () <- (observe t2 t)
                 return (c))
      return (c))

  )

(displayln "FRONT-DOOR ADJUSTMENT EXAMPLE\n")
(displayln "Claim D.1 - Theorem 5.17")
(displayln
"The formula in Theorem 5.17 computes the front-door adjustment
from the work of Pearl et al. in the example given there
(Table 3.1, page 66 - Solution to Study Question 4.3.4).\n")
(printf "The formula computes as follows: ~a~n" (smoking-solution))
(printf "Compare with the implementation at `example-frontdoor.rkt`.~n")
