#lang racket
(require leftdo/monad/norm)
(require leftdo/leftdo)
(require leftdo/leftdo-left)
(require leftdo/do-slide)
(require leftdo/slide-normbag)

(lDo Norm
     coin <- (uniform 'head 'tail)
     (list a-coin awake) <- (uniform (list 'head 'monday) (list 'head 'tuesday) (list 'tail 'monday))
     '() <- (observe coin a-coin)
     return awake)

(rDo Norm
     (list a-coin awake) <- (uniform (list 'head 'monday) (list 'head 'tuesday) (list 'tail 'monday))
     coin <- (uniform 'head 'tail)
     '() <- (observe coin a-coin)
     return awake)


(define (bayes-wake c)
  (match c
    ['head (uniform 'monday 'tuesday)]
    ['tail (uniform 'tuesday)]))

(define (freq-wake c)
  (match c
    ['head (bag 'monday 'tuesday)]
    ['tail (bag 'tuesday)]))


(slideDo Frequentist
    x <~ (bag 'head 'tail)
    d <~ (freq-wake x)
    return x)

(slideDo Frequentist
    x <~ (bag 'head 'tail)
    d <- (bayes-wake x)
    return x)

(slideDo Frequentist
    x <- (uniform 'head 'tail)
    d <~ (freq-wake x)
    return x)

(slideDo Frequentist
    x <- (uniform 'head 'tail)
    d <- (bayes-wake x)
    return x)


