#lang racket

;; An action is a monad and the action of another functor via a Kleisli map.
(struct action (monad act))

(provide (struct-out action))
