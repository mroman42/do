#lang racket

(require do/monad/norm)
(require do/leftdo)
(require do/intervene/syntax-identify-algorithm)
(require do/intervene/syntax)
(require do/intervene/dag)

(define napkin
  (Dag
   'u1 <- '()
   'u2 <- '()
   'w <- '(u1 u2)
   'z <- '(w)
   'x <- '(z u1)
   'y <- '(x u2)
   visible '(w z x y)))

(display "Identify W in WXY")
(normProgram (identify-algorithm '(w) '(w x y) (normProgram 'p) napkin))
(display "Identify Z in Z")
(normProgram (identify-algorithm '(z) '(z) (normProgram 'p) napkin))
(display "Identify Y in WXY")
(normProgram (identify-algorithm '(y) '(w x y) (normProgram 'p) napkin))
(display "Identify XY in WXY")
(normProgram (identify-algorithm '(x y) '(w x y) (normProgram 'p) napkin))
