#lang racket

#| MAGMAD - MAIN

This code is a proof-of-concept implementation of a normalized-by-construction
non-associative probabilistic semantics.

It has been tested under both Racket v8.10[cs] and Racket v9.3[cs]. When
editing, it is recommended to use the official DrRacket IDE that comes
integrated with the Racket installation. Alternatively, Emacs provides a Racket
REPL. Alternatively, `racket main.rkt` executes all of the claims.
|#

(require "example-monty-hall.rkt") 
(require "example-test-problem.rkt")

(printf "DO-NOTATION~n~n")
(printf "Claim C.1 - Definition 4.12.~n")
(printf "Compare with the implementation at `rightDo.rkt`.~n~n")
(printf "Claim C.2 - Definition 4.13.~n")
(printf "Compare with the implementation at `leftDo.rkt`.~n~n")
(printf "Claim C.3 - Remark 4.15.~n")
(printf "Compare with the implementation at `normDo.rkt`.~n~n")

