#lang racket

(require "normDo.rkt")


(define (transition s a)
  (match (cons s a)
    [(cons 's0 'a) (uniform '(s1) '(s2))]
    [(cons 's0 'b) (uniform '(t1))]
    [(cons 's1 'a) (uniform '(t1))]
    [(cons 's1 'b) (uniform '(t1))]
    [(cons 's2 'a) (uniform '(t2))]
    [(cons 's2 'b) (uniform '(t2))]))

(define (observation s)
  (match s
    ['s0 (uniform '(o0))]
    ['s1 (uniform '(o))]
    ['s2 (uniform '(o))]
    ['t1 (uniform '(o1))]
    ['t2 (uniform '(o2))]))


(define (belief d a)
  (do (s) <- d
      (s1) <- (transition s a)
      (o) <- (observation s1)
      return ((do (s1) <- (transition s a)
                 (o1) <- (observation s1)
                 () <- (observe o o1)
                 return (s1)))))
