# Normalized Probabilistic Semantics is Not Associative

This code accompanies "Normalized Probabilistic Semantics is Not Associative"
([arXiv](https://arxiv.org/pdf/2510.01131)). Do-notation is right-associative by
default (e.g., in Haskell's). This code contains an implementation, in the
Racket programming language, of both left-associative and right-associative
do-notation for magmads (monads without the associativity axiom).

The magmad exemplifying this implementation is the normalized distribution
magmad: similar to the monad of subdistributions, but only allowing normalized
distributions.

This implementation provides a calculator using the paper's notation: its
purpose is to check that the computations in the paper are correct and
reproducible. The claims relate to the correctness of these computations and are
modest; the code is not verified nor it formalizes any of the proofs of the
paper beyond checking its computations.


### List of Claims

**Part A - Monty Hall problem**

 - **Claim A.1 - Example 4.17 and Figure 1.** The two programs (leftDo/rightDo)
   for the Monty Hall problem yield two different solutions. This is claimed in
   Example 4.17. Moreover, these correspond to the two programs where the code
   is manually associated to the left or to the right. This is claimed in Figure
   1, and marked with an inequality.

 - **Claim A.2 - Section 1.1.** The first program of Figure 1 matches the
   first computation block of Section 1.1. This is claimed at the end of Section
   1.1.

 - **Claim A.3 - Section 1.1.** The the second program of Figure 1 matches the
   second computation block of Section 1.1. This is claimed at the end of
   Section 1.1.


**Part B - Unclear test problem**

 - **Claim B.1 - Section 1.2.** The solution of the single test problem is the
   one provided at the end of the first computation block of Section 1.2.

 - **Claim B.2 - Figure 2.** The two programs for the Unclear Test problem yield
   two different solutions. This is claimed in Figure 2 and marked with an
   inequality; it is also claimed at the end of Section 1.2.

 - **Claim B.3 - Section 1.2.** The first program of Figure 2 matches the second
   computation block of Section 1.2. This is claimed at the end of Section 1.2.
   
 - **Claim B.4 - Section 1.2.** The second program of Figure 2 matches the third
   computation block of Section 1.2. This is claimed at the end of Section 1.2.
   


### Download, installation, and sanity-testing instructions


#### Download

This artifact consists of a single directory of Racket source code.

The Racket platform can be downloaded from the official Racket repository
<https://download.racket-lang.org/>. Alternatively, a compatible version of
Racket may be packaged for your OS. The code has been tested with Racket
v9.3[cs] and Racket v8.10[cs]; we expect all intermediate versions to work. This
artifact is a zero-dependency library: it does not require any extra library
beyond the standard installation of Racket and only uses `#lang racket`.

This source code can be downloaded from the following GitHub repository
<https://github.com/mroman42/do/tree/popl>, specifically, from its "popl"
branch.


#### Installation

The Racket platform can be installed following the instructions at the official
Racket repository, which vary by OS. The Racket platform provides a `racket`
command to run the code and a `drracket` command to start a dedicated IDE. No
further installation is required.


#### Sanity-testing instructions





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

In recent joint work with Di Lavore and Széles, we show how this normalization
magmad arises as an incomplete distributive law.

 - [The Magmoid of Normalized Stochastic Kernels.](https://arxiv.org/pdf/2510.01131)

## Causal inference


#### Simpson's paradox (simplified backdoor criterion)

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

#### Smoking causes cancer (frontdoor criterion)

This is the paradigmatic problem of causal inference. Given observational data
tracking the smoking habits, the accumulation of tar in the lungs and the
incidence of cancer: can we prove that smoking causes cancer? In particular, can
we prove this even if we assume that there may be some gene that both
predisposes people to smoke and to develop cancer?

``` Racket
(define survey
  (distribution
     ['(smoker tar nocancer)    323/800]
     ['(smoker tar cancer)       57/800]
     ['(nonsmoker tar nocancer)    1/800]
     ['(nonsmoker tar cancer)     19/800]
     ['(smoker notar nocancer)   18/800]
     ['(smoker notar cancer)      2/800]
     ['(nonsmoker notar nocancer) 38/800]
     ['(nonsmoker notar cancer)  342/800]))
     
(Intervene survey
  WithModel (do
    gene <- ()
    smoking <- (gene)
    tar <- (smoking)
    cancer <- (gene tar)
    visibles (smoking tar cancer))
  Setting (smoking) To ('nonsmoker) In (cancer))
```

The intervention expands to the following code. Note however that we do not need
to reason manually: we use an internal implementation of the identifiability
algorithm.

``` Racket
(do 
      (zp) <- (do 
                  (x z y) <- survey
                  () <- (observe i x)
                  return (z))
      (xp) <- (do 
                  (x z y) <- survey
                  return (x))
      (y)  <- (do 
                  (x z y) <- survey
                  () <- (observe x xp)
                  () <- (observe z zp)
                  return (y))
      return (y))
```


#### Napkin problem

This last problem is another classic of causal inference that illustrates a
less-trivial case for the identifiability algorithm. It is usually presented
without an explanation.

``` Racket
(Intervene p
  WithModel (do
                u1 <- ()
                u2 <- ()
                w <- (u1 u2)
                z <- (w)
                x <- (z u1)
                y <- (x u2)
                visible (w z x y))
  Setting (x) To ('p) In (y))
```

This code models a four-variable distribution and derives the effect of the
variable X on the variable Y. It yields the following code.

``` Racket
(do 
  (x16 y17) <- (do 
      (x13 w14 y15) <- (do 
          (w) <- (do 
              (w1 z2 x3 y4) <- p
              return (w1))
          (x) <- (do 
              (w5 z6 x7 y8) <- p
              () <- (observe z z6)
              () <- (observe w w5)
              return (x7))
          (y) <- (do 
              (w9 z10 x11 y12) <- p
              () <- (observe x x11)
              () <- (observe z z10)
              () <- (observe w w9)
              return (y12))
          return (x w y))
      return (x13 y15))
  () <- (observe x x16)
  return (y17))
```
