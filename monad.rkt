#lang racket

;; monad.rkt

;; This file provides the interface for monads (return, bind) and
;; implementations for multiple basic monads (List, Identity, Maybe, ...).
;; The distribution monad is implemented separately (subdistributions.rkt).

(struct monad (return bind map))


;; Continuation monad.
;(define Cont
 ; (monad 
   ;; return : x -> (x -> r) -> r
  ; (λ (x) (λ (f) (f x)))
   ;; bind : ((x -> r) -> r) ->
   ;;        (x -> ((y -> r) -> r)) ->
   ;;        ((y -> r) -> r)
  ; (λ (d f) (λ (k) (d (λ (x) ((f x) k)))))
  ; ))

(define (monad-join M)
  (lambda (mmx)
    ((monad-bind M) mmx (lambda (x) x))))

   
(provide (struct-out monad))
(provide monad-join)


