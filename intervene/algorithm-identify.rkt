#lang racket

(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/syntax-helpers)
(require do/intervene/derive-conditionals)
(require do/intervene/derive-c-component-decomposition)

#| IDENTIFY ALGORITHM.

We assume that Ts and Cs have both a single component.

|#
(define (algorithm-identify q g ts cs)

  ;; #1. Compute A = An(C){G{T}}, the ancestors of C in G{T}.
  (define ancestors
    (dag-visible-ancestors cs (dag-restricted ts g)))

  ;; 2. If A = C, then output the marginal.
  (if (equal-sets? ancestors cs)
      (normMarginal q ts cs)

      ;; #3. If A = T, then output failure
      (if (equal-sets? ancestors ts)
          (error "non-identifiable")

          ;; #4. In any other case, we use a recursive call.
          ;; 4.1. Assume that, in G{A}, C is contained in a single c-component "t-new".
          ;; 4.2. Compute the marginal q[ancestors].
          ;; 4.3. Compute q[t-new] by c-component decomposition, from q[ancestors].
          ;; 4.4. Output IDENTIFY(C,t-new,q[t-new]).
          (let* ([t-new  (dag-c-component-of (first cs) (dag-restricted ancestors g))]
                 [q-new  (c-component-decomposition (normMarginal q ts ancestors) ancestors t-new)])
            (algorithm-identify q-new (dag-restricted t-new g) t-new cs)))))
