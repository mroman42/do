#lang racket

;; UNCLEAR TEST PROBLEM.
;;
;; The unclear test problem is a variant of the single test problem where the
;; result cannot be established clearly (imagine, for instance, that it is dark
;; and we cannot clearly see what the result is).
;;
;; Consider an illness with a certain known prevalence, for which we have a test
;; with a certain, known, specificity and sensibility. Imagine it is dark and we
;; only give a certain probability to having obtained a positive result. For a
;; randomly selected patient that tests negative, what is the posterior
;; probability of illness?
;;
;; REFERENCES.
;;  - https://arxiv.org/pdf/1807.05609

(require do/monad/norm)
(require do/notation/leftdo)
(require do/do-do)


;; This is the example in the original paper (https://arxiv.org/pdf/1807.05609).

(define (test patient)
  (match patient
    ['ill      (distribution ['positive 90/100] ['negative 10/100])]
    ['healthy  (distribution ['positive  5/100] ['negative 95/100])]))

(define afterPositiveTest
  (leftDo Norm
       patient <- (distribution ['ill 1/100] ['healthy 99/100])
       result <- (test patient)
       '() <- (observe result 'positive)
       return patient))

(define pearlUnclearTest
  (leftDo Norm
       observation <- (distribution ['positive 80/100] ['negative 20/100])
       patient <- (distribution ['ill 1/100] ['healthy 99/100])
       result <- (test patient)
       '() <- (observe result observation)
       return patient))


(define jeffreyUnclearTest
  (leftDo Norm
       observation <- (distribution ['positive 80/100] ['negative 20/100])
       patient <- (leftDo Norm
                       patient <- (distribution ['ill 1/100] ['healthy 99/100])
                       result <- (test patient)    
                       '() <- (observe result observation)
                       return patient)
       return patient))


;; This is the same example with easier numbers.

(define prevalence
  (distribution ['ill 1/3] ['healthy 2/3]))

(define uncertainty
  (distribution ['positive 3/4] ['negative 1/4]))

(define (channel patient)
  (match patient
    ['ill      (distribution ['positive 3/4] ['negative 1/4])]
    ['healthy  (distribution ['positive 1/2] ['negative 1/2])]))

(leftDo Norm
     result <- uncertainty
     patient <- prevalence
     test <- (channel patient)
     '() <- (observe test result) 
     return patient)

(leftDo Norm
     result <- uncertainty
     patient <- (leftDo Norm
        patient <- prevalence
        test <- (channel patient)
        '() <- (observe test result)
        return patient)
     return patient)

(leftDo Norm
     patient <- prevalence
     test <- (channel patient)
     patient <- (leftDo Norm
        result <- uncertainty
        '() <- (observe test result)
        return patient)
     return patient)

(do Norm
    result <- uncertainty
    patient <- prevalence
    test <- (channel patient)
    '() <- (observe test result)
     return patient)





