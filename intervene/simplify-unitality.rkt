#lang racket

(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/syntax-helpers)

(define (simplify-unitality p)
  (define (go p)
    (match p
      [(normStatement X F (normReturn Y))
       (if (equal? X Y)
           (simplify-unitality F)
           (normProgram (normStatement X (simplify-unitality F) (normReturn Y))))]
      [q (normProgram q)]))
  (match p
    [(normProgram p) (go p)]))

(provide simplify-unitality)
