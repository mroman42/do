#lang racket

{require {for-syntax do/intervene/id-algorithm}}
{require {for-syntax do/intervene/syntax}}

;; (define (Identify y x g p)
;;   (normReify (normProgram (id-algorithm y x g (normProgram p)))))

;; Here is where it becomes important to use syntax on our algorithm.
;; The dream is to do the following.
;; (WithModel
;;   u <- ()
;;   x <- (x y)
;;   (visible x y w)
;; Identify x -> y In p) x
;; 
{define-syntax (Identify stx)
  (syntax-case stx ()
    [(_ y x g p)
     (with-syntax ([stx-transformed (normReify (normProgram (id-algorithm #'y #'x #'g (normProgram #'p))))])
       #'stx-transformed)])}
  ;(normReify (normProgram (id-algorithm y x g (normProgram p))))}

;; {define-syntax-rule (Identify stx)
;;   (with-syntax* ([(Identify y x g p) stx]
;;                 [stx-transformed (normReify (normProgram (id-algorithm y x g (normProgram p))))])
;;     #'stx-transformed)}

(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/id-algorithm)
(require do/monad/norm)
(require do/leftdo)

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

(define (p)
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
   'u1 <- '()
   'u2 <- '()
   'w <- '(u1 u2)
   'z <- '(w)
   'x <- '(z u1)
   'y <- '(x u2)
   visible '(w z x y)))

(Identify '(y) '(x) napkin 'p)
