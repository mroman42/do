# Belief Construction in the Magmadic Metalanguage

This is a concise implementation the belief construction over a magmadic
metalanguage for normalized distributions.

``` racket
(define (belief-construction transition observation)
  (lambda (d)
    (do (s) <- d
        (s1) <- (transition s)
        (o) <- (observation s1)
        return ((do (s1) <- (transition s)
                    (o1) <- (observation s1)
                    () <- (observe o o1)
                    return (s1))))))
```


## Example - Initial POMDP

![Alt text](./pomdp-start.svg)

The image of the original POMDP is generated from the following code.

``` racket
(define (example s)
  (match s
    ['s0 (uniform '(s1) '(s2) '(s3))]
    ['s1 (uniform '(t1))]
    ['s2 (uniform '(t2))]
    ['s3 (uniform '(t2))]
    ['t1 (uniform '(t1))]
    ['t2 (uniform '(t2))]))

(define (example-observation s)
  (match s
    ['s0 (uniform '(o0))]
    ['s1 (uniform '(o))]
    ['s2 (uniform '(o))]
    ['s3 (uniform '(o))]
    ['t1 (uniform '(o1))]
    ['t2 (uniform '(o2))]))


(mdp example 's0)
```


## Example - Belief MDP

![Alt text](./mdp-belief.svg)

The image of the associated belief MDP is generated from the following code.

``` racket
(mdp (belief-construction example example-observation) (uniform '(s0)))
```

Support for MDP whose reachable part is not finite requires us to stop the computation and it is not available in this version.

## References
 - From Coalgebraic Determinization to Belief Construction for Partial Observability. Kori, Watanabe (CONCUR 2026). 



