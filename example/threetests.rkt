#lang racket

(require leftdo/leftdo)
(require leftdo/monad)
(require leftdo/monad-norm)
(require rebellion/collection/multiset)

;; https://arxiv.org/pdf/2309.07053
;; Prevalence 5%
;; Sensitivity 90%
;; Specificity 95%
(define (test patient)
  (match patient
    ['ill      (distribution ['positive 90/100] ['negative 10/100])]
    ['healthy  (distribution ['positive  5/100] ['negative 95/100])]))



(define threeTest
  (lDo Norm
       individual <- (distribution ['ill 5/100] ['healthy 95/100])
       result1 <- (test individual)
       result2 <- (test individual)
       result3 <- (test individual)
       u1 <- (observe result1 'positive)
       u2 <- (observe result2 'positive)
       u3 <- (observe result3 'negative)
       '() <- (uniform u1 u2 u3)
       return individual))

(define threeTestJ
  (lDo Norm
       individual1 <- (distribution ['ill 5/100] ['healthy 95/100])
       individual2 <- (distribution ['ill 5/100] ['healthy 95/100])
       individual3 <- (distribution ['ill 5/100] ['healthy 95/100])
       result1 <- (test individual1)
       result2 <- (test individual2)
       result3 <- (test individual3)
       '() <- (observe result1 'positive)
       '() <- (observe result2 'positive)
       '() <- (observe result3 'negative)
       individual <- (uniform individual1 individual2 individual3)
       return individual))




;;;;;;;; Experiments

(define jex1
  (rDo Norm
       individual <- (distribution ['ill 5/100] ['healthy 95/100])
       individual <- (rDo Norm
                   world <- (uniform 'positive 'positive 'negative)
                   individual <- (lDo Norm
                               result <- (test individual)
                               '() <- (observe result world)
                               return individual)
                   return individual)
       return individual))

(define jex2
  (lDo Norm
       world <- (uniform 'positive 'positive 'negative)
       individual <- (lDo Norm
                          individual <- (distribution ['ill 5/100] ['healthy 95/100])
                          result <- (test individual)
                          '() <- (observe result world)
                          return individual)
       return individual))


       



(define threeTestMult
  (lDo Norm
       individual <- (distribution ['ill 1/20] ['healthy 19/20])
       result1 <- (test individual)
       result2 <- (test individual)
       result3 <- (test individual)
       '() <- (observe (multiset result1 result2 result3)
                       (multiset 'positive 'positive 'negative))
       return individual))


(define threeTestMultRdo
  (rDo Norm
       individual <- (distribution ['ill 1/20] ['healthy 19/20])
       result1 <- (test individual)
       result2 <- (test individual)
       result3 <- (test individual)
       '() <- (observe (multiset result1 result2 result3) (multiset 'positive 'positive 'negative))
       return individual))

(define pearlTests
  (lDo Norm
       result <- (distribution ['positive 2/3] ['negative 1/3])
       individual <- (distribution ['ill 1/20] ['healthy 19/20])
       testing <- (test individual)
       '() <- (observe testing result)
       return individual))

(define jeffreyTests
  (rDo Norm
       result <- (distribution ['positive 2/3] ['negative 1/3])
       individual <- (distribution ['ill 1/20] ['healthy 19/20])
       testing <- (test individual)
       '() <- (observe testing result)
       return individual))

       
