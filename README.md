# Normalized Probabilistic Semantics is Not Associative

This Racket code accompanies "Normalized Probabilistic Semantics is Not
Associative".

Do-notation is right-associative by default (e.g., in Haskell). This code
contains an implementation, in the Racket programming language, of both
left-associative and right-associative do-notation for magmads: monads without
the associativity axiom. The magmad exemplifying this implementation is the
normalized distribution magmad: similar to the monad of subdistributions, but
only allowing normalized distributions.

This implementation provides a calculator using the paper's notation: its
purpose is to check that the computations in the paper are correct and
reproducible. The claims relate to the correctness of these computations and are
modest; the code is not verified nor it formalizes any of the proofs of the
paper beyond checking its computations.

Finally, note that the reviewers have suggested to change the notation and
descriptions of the computations for the first version of the article. The
version of the article that we provide with this submission is the updated
version: the implementation has been modified so that its notation matches this
updated version. The public version on the arXiv
<https://arxiv.org/pdf/2510.01131> contains the previous notation at the time of
review.


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


**Part C - Do-notation**

 - **Claim C.1 - Definition 4.12.** Definition 4.12 coincides exactly with the
   implementation of `rightDo` (in `rightDo.rkt`). Note that, in the paper,
   `magmad-bind` is denoted by the greek letter beta and `magmad-return` is
   denoted by the greek letter eta; note that we use Racket's `match-lambda`
   instead of a lambda. This is claimed in Remark 4.15, and implicitly for the
   rest of the claims.

 - **Claim C.2 - Definition 4.13.** Definition 4.13 coincides exactly with the
   implementation of `leftDo` (in `leftDo.rkt`). This is claimed in Remark 4.15,
   and implicitly for the rest of the claims.

 - **Claim C.3 - Remark 4.15.** The notation `do` introduced in Remark 4.15
   coincides with the notation provided by `normDo.rkt`. Note that, in the paper
   the `Norm` magmad is denoted by `MD`.


**Part D - Front-door adjustment formula**

 - **Claim D.1 - Theorem 5.17.** The formula in Theorem 5.17 computes the
   front-door adjustment from the work of Pearl et al. in the example given there.
   
   > Causal Inference in Statistics: A Primer
   > Pearl, Glymour and Jewell.
   > Table 3.1, page 66 - for the data.
   > Solution to Study Question 4.3.4 in the Solution Manual.

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

You may check the version of your installation by running `racket --version`.
You should get the message "Welcome to Racket v9.3[cs]", or similar.


#### Sanity-testing instructions

All of the claims can be tested by running the main file (`main.rkt`). The
required command is `racket main.rkt`. which should be run from the directory
containing all of the source code. The output of `racket main.rkt` is the
complete list of claims, together with the output of the computations supporting
those claims. You main also run separately the two example files,
`example-monty-hall.rkt` and `example-test-problem.rkt`, which contain each one
of the sections of the claims.

To facilitate the reading, you may want to use "less": `racket main.rkt |
less`.


#### Evaluation instructions

The complete artifact can be run with the single command `racket main.rkt`. The
code can be explored using any text editor or the dedicated DrRacket IDE that
comes with the Racket platform.

 - You may want to check that the results produced by `racket main.rkt`
   correspond to those described on the paper.
 - You may want to check that the two example files, `example-monty-hall.rkt`
   and `example-test-problem.rkt`, contain the pieces of code described in
   Figures 1 and 2.




### Additional artifact description

The artifact contains 9 Racket source code files.

 - `main.rkt`: Loads and runs all example files.

 - `example-monty-hall.rkt`: Runs the computations for the Monty Hall example
   in Section 1.1 of the paper.

 - `example-test-problem.rkt`: Runs the computations for the Unclear Test
   example in Section 1.2 of the paper.

 - `example-frontdoor.rkt`: Runs the computations for the front-door adjustment
   formula example.

 - `magmad.rkt`: Declares the interface of a magmad (a monad without the
   associativity axiom).

 - `distributions.rkt`: Provides a naive implementation of probabilistic
   distributions as lists of element-probability pairs.
 
 - `rightDo.rkt`: Provides an implementation of left-associating do-notation.

 - `leftDo.rkt`: Provides an implementation of left-associating do-notation.
   Depends on the implementation of right-associating do-notation.
   
 - `normDo.rkt`: Provides an implementation of left-associating do-notation
   specifically for the normalized distributions magmad.
 
 - `format-strike.rkt`: Optionally enables displaying cancelled terms of a
   distributions with striked-through text. It does not necessarily work on
   older terminals, the DrRacket environment, or the less command; it is
   disabled by default.
