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
          ['u (distribution ['p 1/7] ['q 6/7])]
          ['v (distribution ['p 1/3] ['q 2/3])])]))

(define (k u2 x)
  (match u2
    ['a (match x
          ['p (distribution ['p 1/4] ['q 3/4])]
          ['q (distribution ['p 2/5] ['q 3/5])])]
    ['b (match x
          ['p (distribution ['p 1/9] ['q 8/9])]
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


(define actual-intervention
(lDo Norm
     u1 <- sigma1
     u2 <- sigma2
     w <- (f u1 u2)
     z <- (g w)
     x <- (lDo Norm
               x <- (h u1 z)
               '() <- (observe x 'p)
               return x)
     y <- (k u2 x)
     return y))



(define jacobs-solution
(lDo Norm
     w <- (lDo Norm
               (list w z x y) <- p
               return w)
     z <- (lDo Norm
               (list w z x y) <- p
               return z)
     (list x y) <- (lDo Norm
               (list w0 z0 x y) <- p
               '() <- (observe w w0)
               '() <- (observe z z0)
               return (list x y))
     '() <- (observe x 'p)
     return y))


(define jacobs-non-solution
(lDo Norm
     w <- (lDo Norm
               (list w z x y) <- p
               return w)
     z <- (lDo Norm
               (list w z x y) <- p
               return z)
     y <- (lDo Norm
               (list w0 z0 x y) <- p
               '() <- (observe w w0)
               '() <- (observe z z0)
               '() <- (observe x 'p)
               return y)
     return y))


(define alternative-solution
(lDo Norm
     w <- (lDo Norm
               (list w z x y) <- p
               return w)
     z <- (lDo Norm
               (list w0 z x y) <- p
               '() <- (observe w0 w)
               return z)
     (list x y) <- (lDo Norm
               (list w0 z0 x y) <- p
               '() <- (observe w w0)
               return (list x y))
     '() <- (observe x 'p) 
     return y))

(define modified-solution
(lDo Norm
     (list x y) <- (lDo Norm
                        w <- (lDo Norm
                                  (list w z x y) <- p
                                  return w)                   
                        z <- (lDo Norm
                                  (list w z x y) <- p
                                  return z)
                        (list x y) <- (lDo Norm
                                           (list w0 z0 x y) <- p
                                           '() <- (observe w w0)
                                           '() <- (observe z z0)
                                           return (list x y))
                        return (list x y))
     '() <- (observe x 'p)
     return y))

(define my-solution
(lDo Norm
     z <- (lDo Norm
               (list w0 z x y) <- p
               return z)
     y <- (lDo Norm
               (list x1 y) <- (lDo Norm
                                   w <- (lDo Norm
                                             (list w z x y) <- p
                                             return w)                   
                                   x1 <- (lDo Norm
                                              (list w0 z0 x y) <- p
                                              '() <- (observe w w0)
                                              '() <- (observe z z0)
                                              return x)
                                   y <- (lDo Norm
                                             (list w0 z0 x0 y) <- p
                                             '() <- (observe w w0)
                                             '() <- (observe z z0)
                                             '() <- (observe 'p x0)
                                             return y)
                                   return (list x1 y))
               '() <- (observe x1 'p)
               return y)
     return y))

(define my-other-solution
(lDo Norm
     ;; ID(Y <- X; WZXY)
     ;; IDENTIFY(W; WXY)
     w <- (lDo Norm
               (list w0 z0 x0 y0) <- p
               return w0)
     ;; IDENTIFY(Z; Z), input on w
     z <- (lDo Norm
               (list w1 z1 x1 y1) <- p
               '() <- (observe w1 w)
               return z1)
     ;; IDENTIFY(Y; WXY), input on z; intervention on x
     y <- (lDo Norm
               ;; IDENTIFY(XY; WXY), input on z; (!) non-input w
               wa <- (lDo Norm
                         (list w z x y) <- p
                         return w)
               x1 <- (lDo Norm
                          w <- (lDo Norm
                                    (list w z x y) <- p
                                    return w)
                          x1 <- (lDo Norm
                                     (list w0 z0 x y) <- p
                                     '() <- (observe w w0)
                                     '() <- (observe z z0)
                                     return x)
                          '() <- (observe w wa)
                          return x1)
               y <- (lDo Norm
                         w <- (lDo Norm
                                   (list w z x y) <- p
                                   return w)
                         x2 <- (lDo Norm
                                    (list w0 z0 x y) <- p
                                    '() <- (observe w w0)
                                    '() <- (observe z z0)
                                    return x)
                         y <- (lDo Norm
                                   (list w0 z0 x0 y) <- p
                                   '() <- (observe w w0)
                                   '() <- (observe z z0)
                                   '() <- (observe 'p x0)
                                   return y)
                         '() <- (observe w wa)
                         '() <- (observe x1 x2)
                         return y)
               '() <- (observe x1 'p)
               return y)
     return y))


(define automatic-solution
(lDo Norm
     ;; ID(Y <- X; WZXY)

     ;; IDENTIFY(W; WXY)
     w <- (lDo Norm
               (list w0 z0 x0 y0) <- p
               return w0)

     ;; IDENTIFY(Z; Z), input on w
     z <- (lDo Norm
               (list w1 z1 x1 y1) <- p
               '() <- (observe w1 w)
               return z1)

     ;; IDENTIFY(Y; WXY), input on z; intervention on x
     y <- (lDo Norm
               w <- (lDo Norm
                         (list w1994 z x1995 y1996) <- p
                         return w1994)
               y <- (lDo Norm
                         (list w1997 z x1998 y1999) <- p
                         '() <- (observe 'p x1998)
                         '() <- (observe w w1997)
                         return y1999)
               return y)
     return y))



(define non-solution
(lDo Norm
     (list x y) <- (lDo Norm
                        (list w z x y) <- p
                        return (list x y))
     '() <- (observe x 'p)
     return y))


(provide p)
