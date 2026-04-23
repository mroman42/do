#lang racket

(require leftdo/leftdo)
(require leftdo/monad-norm)

;; This is the example using urns.

(define prior
  (distribution
   ['red 1/5] ['blue 1/5] ['green 1/5] ['yellow 2/5]))

(define/match (shade c)
  [('red)     (uniform 'bright)]
  [('blue)    (uniform 'dark)]
  [('green)   (uniform 'dark)]
  [('yellow)  (uniform 'bright)])

(define pearl
  (lDo Norm
       r <- (distribution ['dark 7/10] ['bright 3/10])
       c <- prior
       s <- (shade c)
       '() <- (observe s r)
       return c))

(define jeffrey
  (rDo Norm
       r <- (distribution ['dark 7/10] ['bright 3/10])
       c <- prior
       s <- (shade c)
       '() <- (observe s r)
       return c))


