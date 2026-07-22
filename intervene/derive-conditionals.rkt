#lang racket

(require do/intervene/syntax)
(require do/intervene/syntax-helpers)
(require do/intervene/derive-observations)


#| DERIVING CONDITIONALS

The following code derives a conditional distribution. Given a distribution P :
X -> V, with visible variables Vs, with output variables As, and output
variables Bs; it computes the distribution P(A|B) : X + A -> B.

(normConditionals p vs as bs)
  ==
(do
     vs{temporary} <- p
     observe bs bs{temporary}
     return as)

It uses the algebraic disintegration axiom. |#

(define (normConditional p V A B)

  (define temporary-variables
    (map (lambda (x) (syntax->datum x)) (generate-temporaries V)))

  (define (temporary v)
    (define (gettemp v l)
      (match l
        ['() (error "normConditionals: list exhausted" V B)]
        [(cons (cons x y) ls)
         (if (equal? v x) y (gettemp v ls))]))    
    (gettemp v (map cons V temporary-variables)))

  (define (temporaries xs)
    (map (lambda (x) (temporary x)) xs))

  (normProgram
    (normStatement temporary-variables p
    (normObservations B (temporaries B)
    (normReturn (temporaries A))))))

(provide normConditional)

(define (normMarginal P V A)
  (normConditional P V A '()))

(provide normMarginal)


; EXAMPLE
(define (example)
  (normConditional (normProgram #'p) '(x y z a b c) '(x y) '(b c)))
