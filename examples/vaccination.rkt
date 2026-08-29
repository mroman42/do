#lang racket

(require do/intervene/intervene)

(define vaccination
  (distribution
    ['(vaccinated)    990000/1000000]
    ['(unvaccinated)   10000/1000000]))

(define (reactogenicity vaccinationStatus)
  (match vaccinationStatus
    ['vaccinated   (distribution
                      ['(reaction)    1/100]
                      ['(benign)     99/100])]
    ['unvaccinated (distribution
                      ['(benign)    100/100])]))

(define (incidence vaccinationStatus)
  (match vaccinationStatus
    ['vaccinated (distribution ('(healthy) 1/1))]
    ['unvaccinated (distribution ('(healthy) 49/50) ('(ill) 1/50))]))

(define (mortality smallpox reaction)
  (match smallpox
    ['ill     (distribution ('(fatal) 1/5) ('(alive) 4/5))]
    ['healthy (match reaction
                ['reaction (distribution ('(fatal) 1/100) ('(alive) 99/100))]
                ['benign   (distribution ('(alive) 1/1))])]))


;; How many die from vaccination? If we observe that someone died, it is more
;; likely that it was due to the vaccine than the illness itself. From 139
;; deaths, 99 are due to the vaccine, and only 40 are due to smallpox.
(do (vaccinationStatus) <- vaccination
    (reaction) <- (reactogenicity vaccinationStatus)
    (smallpox) <- (incidence vaccinationStatus)
    (death) <- (mortality smallpox reaction)
    () <- (observe death 'fatal)
    return (vaccinationStatus smallpox))

;; But what we really need to ask is what would happen if we were not to
;; vaccinate. Here we get 4000/1000000 deaths, while our original vaccination
;; plan yields 139/1000000 deaths.
(intervene (do (vaccinationStatus) <- vaccination
               (reaction) <- (reactogenicity vaccinationStatus)
               (smallpox) <- (incidence vaccinationStatus)
               (death) <- (mortality smallpox reaction)
               return (vaccinationStatus reaction smallpox death))
           WithModel (do vaccinationStatus <- ()
                         reaction <- (vaccinationStatus)
                         smallpox <- (vaccinationStatus)
                         death <- (reaction smallpox)
                         visibles (vaccinationStatus reaction smallpox death))
           Setting (vaccinationStatus) To ('unvaccinated)
           In (death))

 (do (vaccinationStatus) <- vaccination
    (reaction) <- (reactogenicity vaccinationStatus)
    (smallpox) <- (incidence vaccinationStatus)
    (death) <- (mortality smallpox reaction)
    return (death))
 
