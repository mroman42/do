#lang racket

(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/syntax-conditional)
(require do/precondition)

(define (equal-sets? as bs)
  (equal?
   (list->set as)
   (list->set bs)))


;; Output is a program.
(define (sce ys xs g p)
  
  (define vs (dag-visibles g))

  (precondition (subset? (list->set xs) (list->set vs))
   'separated-component "intervention variables ~v must be visible variables ~v" xs vs)
  (precondition (subset? (list->set ys) (list->set vs))
   'separated-component "output variables ~v must be visible variables ~v" ys vs)

  (define (acc-sce as ys xs g)
    (match g
      [(dagDependency u _ h)
       (if (or (member u xs) (not (member u vs)))

           ;; #1. Intervened and hidden variables do not appear.
           (if (not (member u vs))
               (acc-sce as ys xs h) ;; hidden
               (acc-sce (cons u as) ys xs h)) ;; intervened

           ;; #2. Visible non-intervening variables are conditioned upon.
           (normStatement u (normProgram (normConditional u as vs p))
                          (acc-sce (cons u as) ys xs h)))]
      
      [(dagVisible vs) (normReturn ys)]))
  
  (normProgram (acc-sce '() ys xs g)))

(define (sce-until ys xs g p v)
  
  (define vs (dag-visibles g))

  (precondition (subset? (list->set xs) (list->set vs))
   'separated-component "intervention variables ~v must be visible variables ~v" xs vs)
  (precondition (subset? (list->set ys) (list->set vs))
   'separated-component "output variables ~v must be visible variables ~v" ys vs)

  (define (acc-sce as ys xs g)
    (match g
      [(dagDependency u _ h)
       (if (or (member u xs) (not (member u vs)))

           ;; #1. Intervened and hidden variables do not appear.
           (if (not (member u vs))
               (acc-sce as ys xs h) ;; hidden
               (acc-sce (cons u as) ys xs h)) ;; intervened

           ;; #2. Visible non-intervening variables are conditioned upon.
           (normStatement u (normProgram (normConditional u as vs p))
                          (acc-sce (cons u as) ys xs h)))]
      
      [(dagVisible vs) (normReturn ys)]))
  
  (normProgram (acc-sce '() ys xs g)))

(define example
  (sce '(y) '(x)
              (Dag
                'u1 <- '()
                'u2 <- '()
                'w <- '(u1 u2)
                'x <- '(w u1)
                'z <- '(x)
                'y <- '(z u2)
                visible '(w x z y))
              (normProgram 'p)))




(define (identify-algorithm cs ts q g)
  ;; #0. The intervention inputs are those not in C.
  (define ins
    (filter (lambda (x) (not (member x cs))) ts))
  
  ;; #1. Compute A = An(C){G{T}}, the ancestors of C in G{T}.
  (define a
    (dag-visible-ancestors cs
      (dag-restricted ts g)))
  
  ;; #2. If A = C, then output the marginal.
  (if (equal-sets? a cs)
      (normConditionals cs '() ts q)

      ;; #3. If A = T, then output failure.
      (if (equal-sets? a ts)
          (error "non identifiable")

          ;; #4. In any other case, we will need a recursive call. TODO: that
          ;; (first cs) is a bit ugly; I think I rely on the assumption that it
          ;; is one component. TODO: Also, rename variables!
          (let* ([t-new (dag-c-component-of (first cs) (dag-restricted a g))]
                 [r-new (filter (lambda (x) (not (member x t-new))) a)]
                 [q-new (sce t-new r-new (dag-restricted ts g) q)])
            (identify-algorithm cs t-new q-new (dag-restricted t-new g))))))




(provide sce)
(provide identify-algorithm)

