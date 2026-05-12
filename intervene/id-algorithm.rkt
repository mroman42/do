#lang racket

(require do/intervene/dag)
(require do/monad)
(require do/monad/norm)
(require do/leftdo)
(require do/intervene/syntax-identify-algorithm)
(require do/intervene/syntax)
(require do/intervene/syntax-helpers)


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

(define napkin
  (Dag
   'u1 <- (list )
   'u2 <- (list )
   'w <- (list 'u1 'u2)
   'z <- (list 'w)
   'x <- (list 'z 'u1)
   'y <- (list 'x 'u2)
   visible (list 'w 'z 'x 'y)))


(define (line-of v g p)
  (let* ([us (dag-c-component-until v g)]
         [qs (sce us (filter (lambda (x) (not (member x us))) (dag-visibles g)) g p)])
    (identify-algorithm (list v) us qs g)))

;; ID-ALGORITHM
(define (id-algorithm sv tv g p)
  
  (define (acc-id-algorithm sv tv vv g p)
    (match vv
      [(cons v vv)
       (if (member v tv)
           (acc-id-algorithm sv tv vv g p)
           (normStatement (list v) (normProgram (line-of v g p))
                          (acc-id-algorithm sv tv vv g p)))]
      ['()  (normReturn sv)]))

  (acc-id-algorithm sv tv (dag-visibles g) g p))

;(normProgram (line-of 'z napkin (normProgram 'p)))

(define example
  (normProgram
 (id-algorithm '(y) '(x) napkin (normProgram #'p))))

(provide id-algorithm)
