#lang racket

(require symalg)
(require do/monad)

(define (simplify-sexpr s)
  (sexpr (simplify (parse-sexpr s))))

(define s0 0)
(define s1 1)
(define (s+ . args) (simplify-sexpr `(+ ,@args)))
(define (s* . args) (simplify-sexpr `(* ,@args)))
(define (s/ . args) (simplify-sexpr `(/ ,@args)))
(define (sninv? x) #'t)


(provide s0 s1 s+ s* s/ sninv?)
