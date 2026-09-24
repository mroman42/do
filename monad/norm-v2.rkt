#lang racket

(require do/distributions-ring-macro)

(define rationals (ring 0 1 + * zero? /))

(provide-distributions-over-ring rationals)






