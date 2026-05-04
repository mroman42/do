#lang racket

{require {for-syntax leftdo/syntax/do}}

(require leftdo/leftdo)
(require leftdo/leftdo-left)
(require leftdo/monad)
(require leftdo/monad/norm)


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
