#lang racket

(require do/monad)

(struct just (elem) #:transparent)
(struct nothing () #:transparent) 

(define Maybe
  (monad
   ;; return : x -> Maybe x
   (λ (x) (just x))
   ;; bind : Maybe x -> (x -> Maybe y) -> Maybe y
   (λ (xs f)
     (if (just? xs)
         (f (just-elem xs))
         (nothing)))
   ;; map : (x -> y) -> (Maybe x -> Maybe y)
   (λ (f mx)
     (if (just? mx)
         (just (f (just-elem mx)))
         (nothing)))))

(provide
 (struct-out nothing)
 (struct-out just)
 Maybe)
