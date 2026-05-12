#lang racket

(require do/monad/norm)
(require do/leftdo)
(require racket/list)
(require do/intervene/syntax)

{require (for-syntax racket/base)}
{require (for-syntax racket/list)}
{require (for-syntax do/intervene/syntax)}
{require (for-syntax do/intervene/id-algorithm)}
{require (for-syntax do/intervene/dag)}
{require (for-syntax do/leftdo)}

(require do/example/napkin-problem)

(define-syntax (Macro stx)
  (syntax-case stx ()
    [(Identify (_ qx) )
       #`(lDo Norm
              qx <- (uniform 2 3 4)
              return qx)]))

(Macro 'x)


(define-syntax (Macro2 stx)
  (syntax-case stx ()
    [(Identify (_ (x ...)))
       #`(lDo Norm
              (list x ...) <- (uniform (list 2 2) (list 3 4) (list 4 5))
              return (list x ...))]))

;(Macro (list 'x 'y))
(Macro2 '(x y))



;; The problem is that the dag should be defined for-syntax.
;; But I would like to use the dag to 

{begin-for-syntax

  (define (lambda-interventions xs program)
    #`(lambda #,xs #,program))
  
}

(define (lambda-interventions xs program)
  #`(lambda #,xs #,program))


(define-syntax (Identify stx)
  (syntax-case stx ()
    [(Identify p
               (y ...)
               (x ...) g)
     (with-syntax ([stx-transformed
                    (normReifyWithLambda (syntax->datum #'(x ...))
                               (normProgram
                                (id-algorithm
                                 (syntax->datum #'(y ...))
                                 (syntax->datum #'(x ...))
                                 (dagParse #'g)
                                 #'p)))])
       #'stx-transformed)]))




((Identify p (y) (x)
     (Dag u1 <- ()
          u2 <- ()
          w <- (u1 u2)
          z <- (w)
          x <- (z u1)
          y <- (x u2)
          visible (w z x y))) 'p)
