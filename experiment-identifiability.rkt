#lang racket

(require leftdo/leftdo)
(require leftdo/monad)
(require leftdo/monad-norm)


;;; Syntactic approach.

(define-syntax Identify
  (syntax-rules (Identify Input Output WithModel <== visibles)

    ;; Line 1. If there are no interventions, we need a conditional.
    [(Identify p Input '() Output ys
               WithModel
               visibles vs)
     
     (lDo Norm
         vs <- p
         return ys)]
    
    [(Identify p Input '() Output ys
               WithModel
               a <== b
               rest ...)
     
     (Identify p Input '() Output ys
               WithModel
               rest ...)]

    ;; Line 2. Removing the non-ancestors of ys.
    ))




(Identify (uniform 'a 'b) Input '() Output y
          WithModel
          x <== y
          visibles y)



;;; FUNCTIONAL.

(struct dagStatement (output inputs next))
(struct dagReturn (visibles))

(define (getVisibles g)
  (match g
    [(dagStatement xs ys g1) (getVisibles g1)]
    [(dagReturn vs) vs]))

(define my-g
  (dagStatement 'x (list 'a 'b 'c)
  (dagReturn (list 'x))))

(getVisibles my-g)


;; Identifiability problem I(g,p,x,y).
(struct identifying
  (dag probability inputs outputs))

;; Line 1. If x is empty, then return p.
;(define (line1 problem)
;  (if (empty? (identifying-inputs problem))
;      ))

