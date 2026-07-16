#lang racket

(require do/leftdo)
(require do/notation/unbias/leftDo)
(require do/monad/norm)

;; This file shows, as an example, the difference between Pearl and Jeffrey's
;; updates. Pearl associates to the left; Jeffrey associates to the right.

(define prior
  (distribution
   ['(red) 1/7] ['(blue) 3/7] ['(green) 1/7] ['(yellow) 2/7]))

(define/match (shade c)
  [('red)     (distribution ('(bright) 1/3) ('(dark) 2/3))]
  [('blue)    (distribution ('(dark) 2/5) ('(bright) 3/5))]
  [('green)   (uniform '(dark))]
  [('yellow)  (uniform '(bright))])

(define parameter
  (distribution ['(dark) 7/10] ['(bright) 3/10]))


(define pearl
  (do Norm
       (r) <- parameter 
       (c) <- prior
       (s) <- (shade c)
       () <- (observe s r)
       return (c)))

(define jeffrey
  (do Norm
      (r) <- parameter
      (c s) <- (do Norm               
                     (c) <- prior
                     (s) <- (shade c)
                     () <- (observe s r)
                     return (c s))
      return (c)))

;; Pearl, because of associativity
(define catalan1
  (do Norm
      (r) <- parameter
      (c s) <- (do Norm               
                     (c) <- prior
                     (s) <- (shade c)                     
                     return (c s))
      () <- (observe s r)
      return (c)))

;; Jeffrey
(define catalan2
  (do Norm
      (r) <- parameter
      (c s) <- (do Norm               
                     (c) <- prior
                     (s) <- (shade c)
                     () <- (observe s r)
                     return (c s))
      return (c)))

;; Pearl
(define catalan3
  (do Norm
      (r) <- parameter
      (c) <- prior
      (s) <- (shade c)                     
      () <- (observe s r)
      return (c)))

;; Weird
(define catalan4
  (do Norm
      (r) <- parameter
      (c) <- prior
      () <- (do Norm
                (s) <- (shade c)
                () <- (observe s r)
                return ())
      return (c)))

;; Pearl, because of associativity
;; (define catalan5
;;   (do Norm
;;       (r c s) <- (do Norm
;;                  (r) <- parameter
;;                  (c s) <- (do Norm               
;;                      (c) <- prior
;;                      (s) <- (shade c)                     
;;                      return (c s))
;;                  return (r c s))
;;       () <- (observe s r)
;;       return (c)))

(define catalan5
  (do Norm
      (r) <- parameter
      (c) <- (do Norm
                 (c) <- prior
                 () <- (do Norm
                           (s) <- (shade c)
                           () <- (observe s r)
                           return ())
                 return (c))
      return (c)))
