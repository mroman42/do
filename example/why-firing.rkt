#lang racket

(require do/intervene/intervene)

(define data
  (uniform '(fire fire) '(hold hold)))

(define (firing a b)
  (match a ['fire (uniform '(dies))]
           ['hold (match b ['fire (uniform '(dies))] ['hold (uniform '(lives))])]))

;; Probabilistic intervention.
(do
    (a b) <- data
    () <- (observe a 'hold)
    (p) <- (firing a b)
    return (p))

;; Causal intervention.
(do
    (a b) <- (Intervene data
                WithModel (do
                    order <- ()
                    captain <- (order)
                    soldierA <- (captain)
                    soldierB <- (captain)
                    visible (soldierA soldierB))
                Setting (soldierA) To ('hold)
                In (soldierA soldierB))
    (p) <- (firing a b)
    return (p))
