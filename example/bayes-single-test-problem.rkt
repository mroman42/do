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

(require do/monad/norm)
(require do/notation/leftDo)


(define prevalence
  (distribution ['(ill) 1/3] ['(healthy) 2/3]))

(define (channel patient)
  (match patient
    ['ill      (distribution ['(positive) 3/4] ['(negative) 1/4])]
    ['healthy  (distribution ['(positive) 1/2] ['(negative) 1/2])]))

(define (single-test-problem)
  (do Norm
      (patient) <- prevalence
      (test) <- (channel patient)
      () <- (observe 'positive test)
      return (patient)))

(single-test-problem)
