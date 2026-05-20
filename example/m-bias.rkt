#lang racket

(require do/leftdo)
(require do/monad)
(require do/monad/norm)

;; Dummy data for the napkin problem.
(define sigma1
  (distribution ['a 1/2] ['b 1/2]))

(define sigma2
  (distribution ['a 1/3] ['b 2/3]))

(define (f u1 u2)
  (match u1
    ['a (match u2
          ['a (distribution ['x 1/2] ['y 1/6] ['z 1/3])]
          ['b (distribution ['x 1/3] ['y 1/2] ['z 1/6])])]
    ['b (match u2
          ['a (distribution ['x 1/5] ['y 1/5] ['z 3/5])]
          ['b (distribution ['x 1/2] ['y 1/6] ['z 1/3])])]))

(define (g w)
  (match w
    ['x (distribution ['u 1/2] ['v 1/2])]
    ['y (distribution ['u 1/9] ['v 8/9])]
    ['z (distribution ['u 1/2] ['v 1/2])]))

(define (h u1 z)
  (match u1
    ['a (match z
          ['u (distribution ['p 1/7] ['q 6/7])]
          ['v (distribution ['p 1/3] ['q 2/3])])]
    ['b (match z
          ['u (distribution ['p 1/4] ['q 3/4])]
          ['v (distribution ['p 2/5] ['q 3/5])])]))

(define (k u2 x)
  (match u2
    ['a (match x
          ['p (distribution ['p 1/4] ['q 3/4])]
          ['q (distribution ['p 2/5] ['q 3/5])])]
    ['b (match x
          ['p (distribution ['p 1/8] ['q 7/8])]
          ['q (distribution ['p 1/3] ['q 2/3])])]))

(define p
  (lDo Norm
     u1 <- sigma1
     u2 <- sigma2
     w <- (f u1 u2)
     z <- (g w)
     x <- (h u1 z)
     y <- (k u2 x)
     return (list w z x y)))

;; This file presents an example of M-bias.

;; The following does not recover p, because Y is not independent from Z.
(define (recover-p)
  (lDo Norm
       w <- (lDo Norm
                 (list w z x y) <- p
                 return w)
       z <- (lDo Norm
                 (list w0 z x y) <- p
                 '() <- (observe w w0)
                 return z)
       x <- (lDo Norm
                 (list w0 z0 x y) <- p
                 '() <- (observe w w0)
                 '() <- (observe z z0)
                 return x)
       y <- (lDo Norm
                 (list w0 z0 x0 y) <- p
                 '() <- (observe w w0)
                 ;'() <- (observe z z0)
                 '() <- (observe x x0)
                 return y)
       return (list w z x y)))
