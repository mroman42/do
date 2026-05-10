#lang racket

{require {for-syntax do/syntax/do}}

(require do/leftdo)
(require do/monad)
(require do/monad/norm)


;; 4. Macro, with syntax.
{define-syntax (reversedDo stx)
  (with-syntax ([stx-transformed (doReify #'lDo (doReverse (doParse stx)))])
    #'stx-transformed)}


(lDo Norm
     y <- (uniform 'x 'y)
     x <- (uniform y)
     return (list x y))

(reversedDo Norm
  x <- (uniform y)
  y <- (uniform 'x 'y)
  return (list x y))
