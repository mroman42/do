#lang racket

(require do/monad/duals)
(require do/distributions-ring-macro)
(provide-distributions-over-ring (ring d0 d1 d+ d* dninv? d/))
