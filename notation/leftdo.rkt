#lang racket

(require do/monad)
(require do/leftdo)


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

    [(leftDo m
             return v)
     (rightDo m
          return v)]
    
    ))

(provide leftDo)
