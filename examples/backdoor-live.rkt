#lang racket

(require do/intervene/intervene)


(define/table data
  (['(X1 A remission)  28]
   ['(X1 A failure)     2]
   ['(X1 B remission)  78]
   ['(X1 B failure)    12]
   ['(X2 A remission)  33]
   ['(X2 A failure)    12]
   ['(X2 B remission)  10]
   ['(X2 B failure)     5]))

(do (v t o) <- data
    () <- (observe t 'A)
    return (o))

(do (v t o) <- data
    () <- (observe t 'B)
    return (o))


(intervene data
 withModel (do variant <- ()
               treatment <- (variant)
               outcome <- (variant treatment)
               return (variant treatment outcome))
 setting (treatment) to ('A) in (outcome))

(intervene data
 withModel (do variant <- ()
               treatment <- (variant)
               outcome <- (variant treatment)
               return (variant treatment outcome))
 setting (treatment) to ('B) in (outcome))

(intervene-showSyntax data
 withModel (do variant <- ()
               treatment <- (variant)
               outcome <- (variant treatment)
               return (variant treatment outcome))
 setting (treatment) to ('B) in (outcome))








