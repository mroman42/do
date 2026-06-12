#lang racket

(require do/leftdo)
(require do/notation/unbias/leftDo)
(require do/monad/norm)

;; This file shows, as an example, the difference between Pearl and Jeffrey's
;; updates. Pearl associates to the left; Jeffrey associates to the right.

(define prior
  (distribution
   ['(red) 1/5] ['(blue) 1/5] ['(green) 1/5] ['(yellow) 2/5]))

(define/match (shade c)
  [('red)     (uniform '(bright))]
  [('blue)    (uniform '(dark))]
  [('green)   (uniform '(dark))]
  [('yellow)  (uniform '(bright))])

(define pearl
  (do Norm
       (r) <- (distribution ['(dark) 7/10] ['(bright) 3/10])
       (c) <- prior
       (s) <- (shade c)
       () <- (observe s r)
       return (c)))

(define jeffrey
  (do Norm
      (r c s) <- (do Norm
                     (r) <- (distribution ['(dark) 7/10] ['(bright) 3/10])
                     (c) <- prior
                     (s) <- (shade c)
                     return (r c s))
      () <- (observe s r)
      return (c)))


