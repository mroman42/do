#lang racket

(require do/notation/normDo)
(require do/intervene/derive-interventions)
(require do/intervene/syntax)
(require do/intervene/dag)
(require do/intervene/algorithm-id)

; Simpson's paradox example.
; (https://www.tedposton.org/Courses/IDMD401S25L10.pdf)
(define observational
  (distribution
     ['(female control attack)    1/120]
     ['(female control benign)   19/120]
     ['(female treatment attack)  3/120]
     ['(female treatment benign) 37/120]
     ['(male control attack)     12/120]
     ['(male control benign)     28/120]
     ['(male treatment attack)    8/120]
     ['(male treatment benign)   12/120]))

(define (estimate-observation a)
  (do (g d s) <- observational
      () <- (observe s a)
      return (y))

(define (estimate-intervention a)
  (do 
      (u x1 y1) <- observational
      (y) <- (do 
                 (u0 x0 y0) <- observational
                 () <- (observe u0 u)
                 () <- (observe x0 a)
                 return (y0))
      return (y)))

(define (intervene-wait) (estimate-intervention 'control))
(define (intervene-treat) (estimate-intervention 'treatment))



(provide observational)


;; Example using the interventions.
(define simpson
  (Dag
   'gender <- '()
   'drug <- '(gender)
   'heart <- '(gender drug)
   visible '(gender drug heart)))

(define (example-simpson)
  (algorithm-id (normProgram #'observational) simpson '(drug) '(heart)))

(Intervene observational           
 WithModel (do
   gender <- ()
   drug <- (gender)
   heart <- (gender drug)
   return (gender drug heart))
 Setting (drug) To ('treatment)
 In (heart))
