#lang typed/racket

(struct (M) monoid
  ([unit : M]
   [multiplication : (-> M M M)]))

(provide (struct-out monoid))
