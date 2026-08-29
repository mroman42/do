#lang racket

(require do/notation/normDo)
(require do/intervene/derive-interventions)
(require do/intervene/syntax)
(require do/intervene/dag)
(require do/intervene/algorithm-id)


(define data
  (distribution-table
     ['(X1 A remission)  28]
     ['(X1 A failure)     2]
     ['(X1 B remission)  78]
     ['(X1 B failure)    12]
     ['(X2 A remission)  33]
     ['(X2 A failure)    12]
     ['(X2 B remission)  10]
     ['(X2 B failure)     5]))



(displayln "Naive observational reasoning: A and B")
(do (variant treatment outcome) <- data
    () <- (observe treatment 'A)
    return (outcome))
(do (variant treatment outcome) <- data
    () <- (observe treatment 'B)
    return (outcome))

(displayln "X1 variants: A and B")
(do (variant treatment outcome) <- data
    () <- (observe variant 'X1)
    () <- (observe treatment 'A)
    return (outcome))
(do (variant treatment outcome) <- data
    () <- (observe variant 'X1)
    () <- (observe treatment 'B)
    return (outcome))

(displayln "X2 variants: A and B")
(do (variant treatment outcome) <- data
    () <- (observe variant 'X2)
    () <- (observe treatment 'A)
    return (outcome))
(do (variant treatment outcome) <- data
    () <- (observe variant 'X2)
    () <- (observe treatment 'B)
    return (outcome))



(displayln "Variant distribution")
(do (variant treatment outcome) <- data
    return (variant))

(displayln "A intervention.")
(intervene data
           withModel (do variant <- ()
                         treatment <- (variant)
                         outcome <- (variant treatment)
                         return (variant treatment outcome))
           setting (treatment) to ('A) in (outcome))

(displayln "B intervention.")
(do 
  (variant) <- (do 
      (variant1 treatment2 outcome3) <- data
      return (variant1))
  (outcome) <- (do 
      (variant5 treatment6 outcome7) <- data
      () <- (observe 'B treatment6)
      () <- (observe variant variant5)
      return (outcome7))
  return (outcome))

(displayln "Randomized trial estimator.")
(do (arm) <- (uniform '(A) '(B))
    (outcome) <- (intervene data
                  withModel (do variant <- ()
                                treatment <- (variant)
                                outcome <- (variant treatment)
                                return (variant treatment outcome))
                  setting (treatment) to (arm) in (outcome))
    return (arm outcome))

(displayln "Generated probabilistic code.")
(interveneStx data
           withModel (do variant <- ()
                         treatment <- (variant)
                         outcome <- (variant treatment)
                         return (variant treatment outcome))
           setting (treatment) to (arm) in (outcome))
