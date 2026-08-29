#lang racket

(require do/notation/normDo)
(require do/intervene/derive-interventions)
(require do/intervene/syntax)
(require do/intervene/dag)
(require do/intervene/algorithm-id)

;; Three variables.
;;   1) Stone = {small, big}
;;   2) Surgery = {open, closed}
;;   3) Outcome = {success, failure}

(define observationalDataReal
  (distribution
     ['(small open success)     81/700]
     ['(small open failure)      6/700]
     ['(small closed success)  234/700]
     ['(small closed failure)   36/700]
     ['(big open success)      192/700]
     ['(big open failure)       71/700]
     ['(big closed success)     55/700]
     ['(big closed failure)     25/700]))

(define observationalDataNew
  (distribution
     ['(small open success)     8/80]
     ['(small open failure)      2/80]
     ['(small closed success)   40/80]
     ['(small closed failure)   10/80]
     ['(big open success)      10/80]
     ['(big open failure)       5/80]
     ['(big closed success)     1/80]
     ['(big closed failure)     4/80]))

(define observationalDataBest
  (distribution
     ['(small open success)     70/600]
     ['(small open failure)      5/600]
     ['(small closed success)  195/600]
     ['(small closed failure)   30/600]
     ['(big open success)      165/600]
     ['(big open failure)       60/600]
     ['(big closed success)     50/600]
     ['(big closed failure)     25/600]))

(define observationalDataGood
  (distribution
     ['(small open success)     70/360]
     ['(small open failure)      5/360]
     ['(small closed success)  195/360]
     ['(small closed failure)   30/360]
     ['(big open success)       33/360]
     ['(big open failure)       12/360]
     ['(big closed success)     10/360]
     ['(big closed failure)      5/360]))

(define observationalData
  (distribution
     ['(small open success)     28/180]
     ['(small open failure)      2/180]
     ['(small closed success)   78/180]
     ['(small closed failure)   12/180]
     ['(big open success)       33/180]
     ['(big open failure)       12/180]
     ['(big closed success)     10/180]
     ['(big closed failure)      5/180]))



(displayln "Naive observational reasoning: open and closed")
(do (stone surgery outcome) <- observationalData
    () <- (observe surgery 'open)
    return (outcome))
(do (stone surgery outcome) <- observationalData
    () <- (observe surgery 'closed)
    return (outcome))

(displayln "Small stones: open and closed")
(do (stone surgery outcome) <- observationalData
    () <- (observe stone 'small)
    () <- (observe surgery 'open)
    return (outcome))
(do (stone surgery outcome) <- observationalData
    () <- (observe stone 'small)
    () <- (observe surgery 'closed)
    return (outcome))

(displayln "Big stones: open and closed")
(do (stone surgery outcome) <- observationalData
    () <- (observe stone 'big)
    () <- (observe surgery 'open)
    return (outcome))
(do (stone surgery outcome) <- observationalData
    () <- (observe stone 'big)
    () <- (observe surgery 'closed)
    return (outcome))



(displayln "Stone distribution")
(do (stone surgery outcome) <- observationalData
    return (stone))


(displayln "Open intervention.")
(Intervene observationalData
           WithModel (do stone <- ()
                         surgery <- (stone)
                         outcome <- (stone surgery)
                         return (stone surgery outcome))
           Setting (surgery) To ('open) In (outcome))

(displayln "Closed intervention.")
(do 
  (stone) <- (do 
      (stone1 surgery2 outcome3) <- observationalData
      return (stone1))
  (outcome) <- (do 
      (stone5 surgery6 outcome7) <- observationalData
      () <- (observe 'closed surgery6)
      () <- (observe stone stone5)
      return (outcome7))
  return (outcome))

(displayln "Randomized trial estimator.")
(do (r) <- (uniform '(open) '(closed))
    (outcome) <- (Intervene observationalData
                  WithModel (do stone <- ()
                                surgery <- (stone)
                                outcome <- (stone surgery)
                                return (stone surgery outcome))
                  Setting (surgery) To (r) In (outcome))
    return (r outcome))

(require (for-syntax do/intervene/dag))
(define-syntax (dag stx)
  (syntax-case stx ()
    [(_ o <- is more ...)
     (dagDependency (syntax->datum #'o) (syntax->datum #'is) (dagParse #'(dummyDag more ...)))]
    [(_ visible xs) (dagVisible (syntax->datum #'xs))]))


(dag-input
  'stone <- ()
  'surgery <- ('stone)
  'outcome <- ('stone 'surgery)
  visible ('stone 'surgery 'outcome))

(displayln "Generated probabilistic code.")
(InterveneStx observationalData
           WithModel (do stone <- ()
                         surgery <- (stone)
                         outcome <- (stone surgery)
                         return (stone surgery outcome))
           Setting (surgery) To (arm) In (outcome))
