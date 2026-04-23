#lang racket

(require leftdo/monad)
(require leftdo/monad-norm)
(require leftdo/leftdo)

(define (newcomb x)
  (lDo Norm
       action <- (uniform 'oneBox 'twoBox)
       '() <- (observe action x)
       prediction <- (uniform 'oneBox 'twoBox)
       '() <- (observe action prediction)
       return (list action prediction)))
