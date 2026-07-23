#lang racket

(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/syntax-helpers)
(require do/intervene/derive-conditionals)
(require do/intervene/derive-c-component-decomposition)


#| IDENTIFY ALGORITHM.

We assume that Ts and Cs have both a single component.
We assume that Cs is in Ts.
We assume that Q has output variables in Ts.
The graph is used only for computing ancestors.

|#
(define (algorithm-identify-until q G T C x)
  ;; #0. Compute the variables before x.
  (define X (list-before x C))
  
  ;; #1. Compute A = An(C){G{T}}, the ancestors of C in G{T}.
  (define A (dag-visible-ancestors C (dag-restricted T G)))

  ;; 2. If A = C, then output the marginal. However, because now we are
  ;; conditioning until x, we need to only return x and condition on previous
  ;; variables.
  (if (equal-sets? A C)
      (normConditional q T (list x) X)

      ;; #3. If A = T but A /= C, then output failure, as usual.
      (if (equal-sets? A T)
          (error "non-identifiable")

          ;; #4. In any other case, we use a recursive call.
          ;; 4.1. Assume that, in G{A}, C is contained in a single c-component "t-new".
          ;; 4.2. Compute the marginal q[A].
          ;; 4.3. Compute q[t-new] by c-component decomposition, from q[A].
          ;; 4.4. Output IDENTIFY(C,t-new,q[t-new]).
          (let* ([T-new  (dag-c-component-of x (dag-restricted A G))]
                 [q-new  (c-component-decomposition (normMarginal q T A) A T-new)])
            (algorithm-identify-until q-new (dag-restricted T-new G) T-new C x)))))

(provide algorithm-identify-until)

(define (example)
  (algorithm-identify-until
    (normProgram #'p)
    (Dag
       'u <- '()
       'y <- '(u)
       'x <- '(u)
       visible '(x y))
    '(x y) '(x y) 'x))
