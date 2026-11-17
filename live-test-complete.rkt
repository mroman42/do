#lang racket

(require "notation/normDo.rkt")

(define prevalence
  (distribution ['(I) 1/3] ['(H) 2/3]))

(define (test patient)
  (match patient
    ['I  (distribution ['(P) 3/4] ['(N) 1/4])]
    ['H  (distribution ['(P) 1/2] ['(N) 1/2])]))

(define uncertainty
  (distribution ['(P) 3/4] ['(N) 1/4]))


(do (patient) <- prevalence
    (outcome) <- (test patient)
    () <- (observe 'P outcome)
    return (patient))

(do (result) <- uncertainty
    (patient) <- prevalence
    (outcome) <- (test patient)
    () <- (observe result outcome)
    return (patient))

(do (result) <- uncertainty
    (patient) <- (do (patient) <- prevalence
                     (outcome) <- (test patient)
                     () <- (observe result outcome)
                     return (patient))
    return (patient))
