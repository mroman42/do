#lang racket

(require leftdo/monad-normbag)
(require leftdo/leftdo)

;; https://arxiv.org/pdf/2309.07053
;; Sensitivity 90%
;; Specificity 95%
;; Prevalence 5%

(define (test patient)
  (match patient
    ['ill      (distribution ['positive 90/100] ['negative 10/100])]
    ['healthy  (distribution ['positive  5/100] ['negative 95/100])]
    ))

;; Pearl
(frequentist (lDo NormBag
     p <- (distribution ['ill 5/100] ['healthy 95/100])
     r <- (bag 'positive 'positive 'negative)
     t <- (test p)
     '() <- (observe r t)
     return p))

;; Jeffrey
(frequentist (lDo NormBag
     r <- (bag 'positive 'positive 'negative)
     p <- (distribution ['ill 5/100] ['healthy 95/100])
     t <- (test p)
     '() <- (observe r t)
     return p))

