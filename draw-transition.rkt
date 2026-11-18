#lang racket

(require "draw-graphviz.rkt")
(require "normDo.rkt")


(define (distribution-to-edges i d)
  (map (match-lambda* [(list (list (list x) p)) (edge i x p)]) (unwrap d)))

(define (distribution-to-vertices d)
  (map (match-lambda* [(list (list (list x) p)) x]) (unwrap d)))

(define (transition-to-graph transition s)
  (define (go transition acc-vertices acc-edges next-vertices)
    (match next-vertices
      
      ;; If no more vertices, return.
      ['()  (lts acc-vertices acc-edges)]

      ;; If s is a new vertex, let us check if it was already explored
      [(cons s next)
        (if (member s acc-vertices)
            
            ;; If s already explored, we are done.
            (go transition acc-vertices acc-edges next)

            ;; If not explored, add its transitions.
            (go transition
                (cons s acc-vertices)
                (append acc-edges     (distribution-to-edges s (transition s)))
                (append next-vertices (distribution-to-vertices (transition s)))))]))
  
  (go transition '() '() (list s)))


(define (transition s)
  (match s
    ['s0 (uniform '(s1) '(s2))]
    ['s1 (uniform '(t1))]
    ['s2 (uniform '(t2))]
    ['t1 (uniform '(t1))]
    ['t2 (uniform '(t2))]))

;(draw-lts (transition-to-graph transition 's0))

(define (mdp t s0)
  (draw-lts (transition-to-graph t s0)))

(provide mdp)
