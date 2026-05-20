#lang racket

(require do/monad)
(require do/notation/rightDo)


(define-syntax leftDo
  (syntax-rules (<- return)

    [(leftDo m
             x <- f
             y <- g
             rest ...)
     (leftDo m
             (list x y) <- (rightDo m
                                x <- f
                                y <- g
                                return (list x y))
             rest ...)
     ]

    [(leftDo m
             x <- f
             return v)
     (rightDo m
          x <- f
          return v)
    ]

    [(leftDo m return v)
     (rightDo m return v)]
    
    ))


(provide leftDo)
(provide (all-from-out do/monad))
(provide (all-from-out do/notation/rightDo))
