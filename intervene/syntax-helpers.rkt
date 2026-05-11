#lang racket

(define (syntaxSymbol->string x)
  (symbol->string (syntax->datum x)))

(define (syntaxVars->string vs)
  (define (go vs)
    (match vs
      [(cons v vs) (string-append " " (syntaxSymbol->string v) (go vs))]
      ['() ""]))
  (string-append "(list" (go vs) ")"))

(define (equalDatum? x y)
  (equal? (syntax->datum x) (syntax->datum y)))

(define (syntax->datums xs)
  (map (lambda (x) (syntax->datum x)) xs))

(define (memberDatum? x xs)
  (member (syntax->datum x) (syntax->datums xs)))

(provide syntaxSymbol->string)
(provide syntaxVars->string)
(provide syntax->datums)
(provide equalDatum?)
(provide memberDatum?)
