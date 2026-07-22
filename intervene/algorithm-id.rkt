#lang racket

(require racket/match)
(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/syntax-helpers)
(require do/intervene/derive-conditionals)
(require do/intervene/derive-c-component-decomposition)
(require do/intervene/algorithm-identify)


#| ID ALGORITHM (Tian and Shpitser, 2009)

S and T are disjoint.
G outputs variables in V.

|#
(define (algorithm-id p g ts ss)
  
  ; Visible variables in G.
  (define vs (dag-visibles g))
  
  ; Ancestors of S in the graph G{V-T}. These are the D variables in the
  ; algorithm
  (define ds
    (dag-visible-ancestors ss (dag-remove ts g)))

  ; Statement associated to a c-component of D.
  (define (statement-of dc)
    (printf "called ~a\n" g)
    ; sc: component of Dc in the total graph.
    ; qs: identification of the component sc.
    (let* ([sc  (dag-c-component-of-vars dc g)]
           [qs  (c-component-decomposition p vs sc)])
      (algorithm-identify qs g sc dc)))

  (define (statement-components dcs)
    (match dcs
      ['() (normReturn ss)]
      [(cons dc dcs) (normStatement dc (statement-of dc) (statement-components dcs))]))
  
  (normProgram (statement-components (dag-c-components (dag-remove ts g)))))

  ;; ; Statement associated to a variable d.
  ;; (define (statement-until dc d)
  ;;   (let* ([sc  (dag-c-component-of (first ds) g)]
  ;;          [qs  (c-component-decomposition p vs sc)]
  ;;          [dpast (until dc d)])
  ;;     (filter-program (algorithm-identify qs g sc dc) dc dpast)))

  ;; ; Statements missing.
  ;; (define (go dm)
  ;;   (match dm
  ;;     [(cons d dm) (normStatement (list d) (statement-until ds d) (go dm))]
  ;;     ['() (normReturn ss)]))
  
  ;; (normProgram (go ds))
  
  

; Example
(require do/example/data-napkin)

(define napkin
  (Dag
   'u1 <- '()
   'u2 <- '() 
   'w <- '(u1 u2)
   'z <- '(w)
   'x <- '(z u1)
   'y <- '(x u2)
   visible '(w z x y)))

(define (example-napkin)
   (algorithm-id (normProgram #'p) napkin '(z) '(y)))
