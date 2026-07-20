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

(define (c-component-decomposition p as ss)

  (define (go acc vars)
    (match vars
      [(cons u vars-rest)
       (if (member u ss)
           (normStatement (list u) (normConditional p as (list u) acc)
                          (go (cons u acc) vars-rest))
           (if (member u vars)
               (go (cons u acc) vars-rest)
               (go acc vars-rest))) ]

      ['() (normReturn ss)]))

  (normProgram (go '() as)))

(provide c-component-decomposition)
