#lang racket

;; monad.rkt

;; This file provides the interface for monads (return, bind). The
;; implementation of particular monads is separated.

(struct monad (return bind map))

(define (monad-join M)
  (lambda (mmx)
    ((monad-bind M) mmx (lambda (x) x))))

   
(provide (struct-out monad))
(provide monad-join)


