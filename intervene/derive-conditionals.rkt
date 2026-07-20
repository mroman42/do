#lang racket

(require do/intervene/syntax)
(require do/intervene/syntax-helpers)
(require do/intervene/derive-observations)


#| DERIVING CONDITIONALS

The following code derives a conditional distribution. Given a distribution P,
with visible variables Vs, with output variables As, and output variables Bs; it
computes the distribution P(A|B).

(normConditionals p vs as bs)
 ==
(do
     vs{temporary} <- p
     observe bs bs{temporary}
     return as)

It uses the algebraic disintegration axiom. |#



(define (normConditional p vs as bs)

  (define temporary-variables
    (map (lambda (x) (syntax->datum x)) (generate-temporaries vs)))

  (define (temporary v)
    (define (gettemp v l)
      (match l
        ['() (error "normConditionals: list exhausted" vs bs)]
        [(cons (cons x y) ls)
         (if (equal? v x) y (gettemp v ls))]))    
    (gettemp v (map cons vs temporary-variables)))

  (define (temporaries xs)
    (map (lambda (x) (temporary x)) xs))

  (normProgram
  (normStatement temporary-variables p
  (normObservations bs (temporaries bs)
  (normReturn (temporaries as))))))

(provide normConditional)


(define (normMarginal p vs as)
  (normConditional p vs as '()))

(provide normMarginal)
