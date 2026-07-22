#lang racket

(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/derive-conditionals)

#| C-COMPONENT DECOMPOSITION.

This is Lemma 10 by Tian, Shpitser, 2009. It takes a list of topologically
ordered variables, "as", together with a distribution p : X -> A and a subset of
variables that form a c-component, "ss".

(c-component-decomposition p as ss)
  ==
(do
  s1 <- P (s1 given {a < s1 | a in as})
  ...
  sn <- P (sn given {a < sn | a in as})
  return ss)

We expect the p to output variables in as. We expect ss to be a subset of the
as. We use an accumulator function that has all of the variables that appeared
until now and all of the variables that we still need to explore. |#

(define (c-component-decomposition P A S)
  ; We will traverse A. Every time we pick a variable a in A, we see if it is in
  ; S. If it is in S, then we compute a conditional with all previous variables.
  ; If it is not, we attach it to the accumulator. We return S

  (define (go acc vars)
    (match vars
      [(cons a vars-rest)
       (if (member a S)
           (normStatement (list a) (normConditional P A (list a) acc)
                          (go (cons a acc) vars-rest))
           (go (cons a acc) vars-rest)) ]

      ['() (normReturn S)]))

  (normProgram (go '() A)))

(provide c-component-decomposition)


; EXAMPLE
(define (example)
  (c-component-decomposition (normProgram #'p)
                             '(x y z a b c)
                             '(x a c)))
