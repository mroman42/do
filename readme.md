# A Magmadic Metalanguage for Causal Inference

Monadic bind is usually left-associative, but do-notations (e.g., Haskell's)
are right-associative. This is an implementation of both left and right
do-notation for arbitrary monad-like constructions in Racket. 

Of course, left and right-associative do-notation coincide for any monad, but
there are some constructions -- like probabilistic normalization -- that do not
form a monad but only a non-associative monad. For these, we can modulate the
semantics of a do-notation block by changing how statements associate.

## Causal inference

An example is the Monty Hall problem. Let us assume we pick the left door and
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

In recent joint work with Di Lavore and Széles, we show how this normalization
magmad arises as an incomplete distributive law. If you want other examples of
magmoid and are comfortable with category theory, I highly recommend the work on
duploids linked below.

 - [The Magmoid of Normalized Stochastic Kernels.](https://arxiv.org/pdf/2510.01131)
 - [Classical notions of computation and the Hasegawa-Thielecke theorem.](https://arxiv.org/abs/2502.13033)
