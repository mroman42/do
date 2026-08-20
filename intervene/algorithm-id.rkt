#lang racket

(require racket/match)
(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/syntax-helpers)
(require do/intervene/derive-conditionals)
(require do/intervene/derive-c-component-decomposition)
(require do/intervene/algorithm-identify-until)
(require do/notation/normDo)

#| ID ALGORITHM (Tian and Shpitser, 2009)

S and T are disjoint.
G outputs variables in V.

|#
(define (algorithm-id p G T S)
  
  ; Visible variables in G.
  (define V (dag-visibles G))
  
  ; Ancestors of S in the graph G{V-T}. These are the D variables in the
  ; algorithm
  (define D
    (dag-visible-ancestors S (dag-remove T G)))

  ; Statement associated to a variable.
  (define (statement-of x)
    (let* ([Sj (dag-c-component-of-vars (list x) G)]
           [Di (dag-c-component-of-vars (list x) (dag-restricted D G))]
           [QSj (c-component-decomposition p V Sj)])
      (algorithm-identify-until QSj G Sj Di x)))

  ; List all statements.
  (define (go vars)
    (match vars
      [(cons x vars) (normStatement (list x) (statement-of x) (go vars))]
      ['() (normReturn S)]))

  (normProgram (go D)))

(provide algorithm-id)
