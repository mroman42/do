#lang racket

(require leftdo/monad)
(require leftdo/leftdo)


(define-syntax leftDo
  (syntax-rules (<- return)

    [(leftDo m
             x <- f
             y <- g
             rest ...)
     (leftDo m
             (list x y) <- (rDo m
                                x <- f
                                y <- g
                                return (list x y))
             rest ...)
     ]

    [(leftDo m
             x <- f
             return v)
     (rDo m
          x <- f
          return v)
    ]

    [(leftDo m
             return v)
     (rDo m
          return v)]
    
    ))

(provide leftDo)
