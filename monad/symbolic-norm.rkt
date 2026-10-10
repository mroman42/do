#lang racket

(require do/monad/symb)
(require do/distributions-ring-macro)
(provide-distributions-over-ring (ring s0 s1 s+ s* sninv? s/))
