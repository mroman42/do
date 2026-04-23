#lang racket

(require leftdo/monad)

(define List
  (monad
   ;; x -> List x
   (λ (x) (list x))
   ;; List x -> (x -> List y) -> List y
   (λ (xs f) (append-map f xs))
   ;; (x -> y) -> List x -> List y
   (λ (f xs) (map f xs))))

(provide List)
