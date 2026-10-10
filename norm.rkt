#lang racket

(require do/distributions-ring-macro)
(provide-distributions-over-ring (ring 0 1 + * zero? /))

