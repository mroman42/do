#lang racket

;; magmad.rkt

;; This file provides the interface for magmads (return, bind). The
;; implementation of particular magmads is separated, and this version includes
;; only the normalization magmad.

(struct magmad (return bind map))

(define (magmad-join M)
  (lambda (mmx)
    ((magmad-bind M) mmx (lambda (x) x))))

(provide (struct-out magmad))
(provide magmad-join)

