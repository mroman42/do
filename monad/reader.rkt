#lang racket

(require do/monad)

(define Reader
  (monad
     ;; return : x -> (s -> x)
     (λ (x) (λ (s) x))
     
     ;; bind : (s -> x) -> (x -> (s -> y)) -> (s -> y)
     (λ (xs f) (λ (s) ((f (xs s)) s)))

    ;; map : (x -> y) -> ((s -> x) -> (s -> y))
    (λ (f xs) (λ (s) (f (xs s))))))


(provide Reader)
