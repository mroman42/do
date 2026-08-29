#lang racket

(require do/intervene/intervene)

(define p
  (let* ([sigma1 (distribution ['(a) 1/2] ['(b) 1/2])]
         [sigma2 (distribution ['(a) 1/3] ['(b) 2/3])]
         [f (lambda (u1 u2) (match u1
                              ['a (match u2
                                    ['a (distribution ['(x) 1/2] ['(y) 1/6] ['(z) 1/3])]
                                    ['b (distribution ['(x) 1/3] ['(y) 1/2] ['(z) 1/6])])]
                              ['b (match u2
                                    ['a (distribution ['(x) 1/5] ['(y) 1/5] ['(z) 3/5])]
                                    ['b (distribution ['(x) 1/2] ['(y) 1/6] ['(z) 1/3])])]))]
         [g (lambda (w) (match w
                          ['x (distribution ['(u) 4/5] ['(v) 1/5])]
                          ['y (distribution ['(u) 2/9] ['(v) 7/9])]
                          ['z (distribution ['(u) 1/2] ['(v) 1/2])]))]
         [h (lambda (u1 z) (match u1
                             ['a (match z
                                   ['u (distribution ['(p) 1/7] ['(q) 6/7])]
                                   ['v (distribution ['(p) 1/3] ['(q) 2/3])])]
                             ['b (match z
                                   ['u (distribution ['(p) 1/2] ['(q) 1/2])]
                                   ['v (distribution ['(p) 2/5] ['(q) 3/5])])]))]
         [k (lambda (u2 x) (match u2
                             ['a (match x
                                   ['p (distribution ['(p) 2/5] ['(q) 3/5])]
                                   ['q (distribution ['(p) 3/5] ['(q) 2/5])])]
                             ['b (match x
                                   ['p (distribution ['(p) 1/7] ['(q) 6/7])]
                                   ['q (distribution ['(p) 1/4] ['(q) 3/4])])]))])
  (do
     (u1) <- sigma1
     (u2) <- sigma2
     (w) <- (f u1 u2)
     (z) <- (g w)
     (x) <- (h u1 z)
     (y) <- (k u2 x)
     return (w z x y))))

(interveneStx p
  withModel (do
                u1 <- ()
                u2 <- ()
                w <- (u1 u2)
                z <- (w)
                x <- (z u1)
                y <- (x u2)
                return (w z x y))
  setting (x) to ('p) in (y))



