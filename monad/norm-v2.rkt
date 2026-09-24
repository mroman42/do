#lang racket

(require do/distributions-ring-macro)

(define rationals (ring 0 1 + * zero? /))

(provide-distributions-with-ring rationals
                                 validity
                                 map
                                 dist-uniform
                                 dist-map
                                 distribution
                                 Norm
                                 norm-bind
                                 observe
                                 uniform
                                 define/table)


(define-syntax distribution-table
  (syntax-rules ()
    [(_ rest ...)  (from-table (distribution rest ...))]))






