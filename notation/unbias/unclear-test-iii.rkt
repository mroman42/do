#lang racket

(require do/monad/norm)
(require do/notation/unbias/leftDo)

(define-syntax Do
  (syntax-rules (<- return)
    [(Do more ...) (leftDo Norm more ...)]))

(define prevalence
  (distribution ['(ill) 1/3] ['(healthy) 2/3]))

(define uncertainty
  (distribution ['(positive) 3/4] ['(negative) 1/4]))

(define (test patient)
  (match patient
    ['ill      (distribution ['(positive) 3/4] ['(negative) 1/4])]
    ['healthy  (distribution ['(positive) 1/2] ['(negative) 1/2])]))

(Do (result) <- uncertainty
    (patient) <- (Do
       (patient) <- prevalence       
       (ofTest) <- (test patient)
       () <- (observe result ofTest)
       return (patient))
    return (patient))
