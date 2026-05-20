#lang racket

(require do/monad)
(require do/notation/unbias/rightDo)


(define-syntax leftDo
  (syntax-rules (<- return)

    [(leftDo m
             (x ...) <- f
             (y ...) <- g
             rest ...)
     (leftDo m
             (x ... y ...) <- (rightDo m
                                (x ...) <- f
                                (y ...) <- g
                                return (x ... y ...))
             rest ...)
     ]

    [(leftDo m
             (x ...) <- f
             return (v ...))
     (rightDo m
          (x ...) <- f
          return (v ...))
    ]

    [(leftDo m return (v ...))
     (rightDo m return (v ...))]
    
    ))

(define-syntax do
  (syntax-rules (<- return)
    [(do i ...) (leftDo i ...)]))


(provide leftDo do)
