#lang racket

(require "normDo.rkt")
(require "draw-transition.rkt")

;; Belief construction with inputs.
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

(define (belief-construction transition observation)
  (lambda (d)
    (do (s) <- d
        (s1) <- (transition s)
        (o) <- (observation s1)
        return ((do (s1) <- (transition s)
                    (o1) <- (observation s1)
                    () <- (observe o o1)
                    return (s1))))))


;; Belief construction with no inputs.
(define (example s)
  (match s
    ['s0 (uniform '(s1) '(s2))]
    ['s1 (uniform '(t1))]
    ['s2 (uniform '(t2))]
    ['t1 (uniform '(t1))]
    ['t2 (uniform '(t2))]))

(define (example-observation s)
  (match s
    ['s0 (uniform '(o0))]
    ['s1 (uniform '(o))]
    ['s2 (uniform '(o))]
    ['t1 (uniform '(o1))]
    ['t2 (uniform '(o2))]))




