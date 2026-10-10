#lang racket

;; SINGLE TEST PROBLEM.
;;
;; The single test problem is a basic problem in probabilistic inference.
;; Consider an illness with a certain known prevalence, for which we have a test
;; with a certain, known, specificity and sensibility. For a randomly selected
;; patient that tests negative, what is the posterior probability of illness?
;;
;; REFERENCES.
;;  - https://arxiv.org/pdf/1807.05609

(require "distributions.rkt")
(require "normDo.rkt")

(define prevalence
  (distribution ['(I) 1/3] ['(H) 2/3]))

(define (channel patient)
  (match patient
    ['I  (distribution ['(P) 3/4] ['(N) 1/4])]
    ['H  (distribution ['(P) 1/2] ['(N) 1/2])]))

(define (single-test-problem)
  (do
      (patient) <- prevalence
      (test) <- (channel patient)
      () <- (observe 'P test)
      return (patient)))


(define uncertainty
  (distribution ['(P) 3/4] ['(N) 1/4]))

(define (unclear-test-problem)
  (do 
      (result) <- uncertainty
      (patient) <- (do 
          (patient) <- prevalence
          (test) <- (channel patient)
          () <- (observe test result)
          return (patient))
      return (patient)))

(display "Single test problem.")  (displaydist (single-test-problem))
(display "Unclear test problem.") (displaydist (unclear-test-problem))
