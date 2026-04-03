#lang racket

;; An action is a monad and the action of another functor via a Kleisli map.
(struct slide
  (monad      ;; D
   actor      ;; B
   distribute ;; BX x (X -> DY) -> DBY
   morph      ;; BX -> DX
   ))

(provide (struct-out slide))
