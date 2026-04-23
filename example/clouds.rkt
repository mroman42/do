#lang racket

(require leftdo/left-do)
(require leftdo/monad)
(require leftdo/monad-norm)

;; Dummy data for the napkin problem.
(define sigma1
  (distribution ['a 1/2] ['b 1/2]))

(define sigma2
  (distribution ['a 1/3] ['b 2/3]))

(define (f u1 u2)
  (match u1
    ['a (match u2
          ['a (distribution ['x 1/2] ['y 1/3] ['z 1/6])]
          ['b (distribution ['x 1/3] ['y 1/2] ['z 1/6])])]
    ['b (match u2
          ['a (distribution ['x 1/6] ['y 1/3] ['z 1/2])]
          ['b (distribution ['x 1/2] ['y 1/6] ['z 1/3])])]))

(define (g w)
  (match w
    ['x (distribution ['u 1/2] ['v 1/2])]
    ['y (distribution ['u 1/3] ['v 2/3])]
    ['z (distribution ['u 1/5] ['v 4/5])]))

(define (h u1 z)
  (match u1
    ['a (match z
          ['u (distribution ['p 1/2] ['q 1/2])]
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
          ['p (distribution ['p 1/2] ['q 1/2])]
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
     return y)



(display "Jacobs' solution.")
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
     return y)


(display "Jacobs' non-solution.")
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
     return y)


(display "Alternative solution.")
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
     return y)

(display "Modified solution.")
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
     return y)

(display "Non-solution.")
(lDo Norm
     (list x y) <- (lDo Norm
                        (list w z x y) <- p
                        return (list x y))
     '() <- (observe x 'p)
     return y)

