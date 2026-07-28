# A Magmadic Metalanguage for Causal Inference

Monadic bind is usually left-associative, but do-notations (e.g., Haskell's)
are right-associative. This is an implementation of both left and right
do-notation for arbitrary monad-like constructions in Racket. 

Of course, left and right-associative do-notation coincide for any monad, but
there are some constructions -- like probabilistic normalization -- that do not
form a monad but only a non-associative monad. For these, we can modulate the
semantics of a do-notation block by changing how statements associate.

In recent joint work with Di Lavore and Széles, we show how this normalization
magmad arises as an incomplete distributive law.

 - [The Magmoid of Normalized Stochastic Kernels.](https://arxiv.org/pdf/2510.01131)

## Causal inference

#### Monty Hall problem
In the Monty Hall problem, let us assume we pick the left door and
that the host announces the middle door. If we associate to the left, we obtain
the usual solution.

``` Racket
(do (car) <- (uniform (list 'left 'middle 'right))
    (choice) <- (uniform (list 'left 'middle 'right))
    (announce) <- (host car choice)
    () <- (observe choice 'left)
    () <- (observe announce 'middle)
    return (car))
>>> '((left 1/3) (right 2/3))
```

However, if we associate to the right, we obtain the solution that assumes that the host intervenes in the program.

``` Racket
(do (car) <- (uniform (list 'left 'middle 'right))
    (choice) <- (uniform (list 'left 'middle 'right))
    (announce) <- (host car choice)
    () <- (observe choice 'left)
    () <- (observe announce 'middle)
    return (car))
>>> '((left 1/2) (right 1/2))
```


#### Simpson's paradox

Imagine we have observational data for a drug and a side effect we want to
avoid. We have a table recording whether a patient was being treated or not and
whether the side effect occurred or not. Traditionally, these tables are split
by demographics (e.g., it is common to find them segregated by gender).

How could we estimate if the drug causes the side effect? A naive estimator
would be the following.

``` Racket
(define (estimate-observation a)
  (do (g d s) <- observational
      () <- (observe s a)
      return (y))
```

However, the correct estimator is the following one.

``` Racket
(define (estimate-intervention a)
  (do 
      (u x1 y1) <- observational
      (y) <- (do 
                 (u0 x0 y0) <- observational
                 () <- (observe u0 u)
                 () <- (observe x0 a)
                 return (y0))
      return (y)))
```

These estimators are difficult to find manually, so we provide an implementation of Tian and Pearl's identification algorithm (see also Tian and Shpitser). We simply state the problem, our modelling of it, and the intervention we need to compute. Internally, this code is rewritten to the previous snippet.

``` Racket
(Intervene observational           
 WithModel (do
   gender <- ()
   drug <- (gender)
   heart <- (gender drug)
   return (gender drug heart))
 Setting (drug) To ('treatment)
 In (heart))
```
