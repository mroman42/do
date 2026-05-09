#lang racket

(require graph)

;; Follows the graph-lib library example.
(define (partition-by-relation items rel?)
  (define g (unweighted-graph/undirected '()))
  (for ([item items]) (add-vertex! g item))
  (for* ([i items] [j items]) (when (rel? i j) (add-edge! g i j)))
  (cc g))

;; Example:
;;(partition-by-relation '(1 2 3 4 5 6) (lambda (a b) (equal? (even? a) (even? b))))
;; Returns: '((6 4 2) (5 3 1))

(provide partition-by-relation)
