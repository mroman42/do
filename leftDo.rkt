#lang racket

(require do/monad)
(require do/notation/rightDo)


(define-syntax leftDo
  (syntax-rules (<- return)

    [(leftDo m
             (x ...) <- m1
             (y ...) <- m2
             rest ...)
     (leftDo m
             (x ... y ...) <- (rightDo m
                                (x ...) <- m1
                                (y ...) <- m2
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
(provide (all-from-out do/monad))
(provide (all-from-out do/notation/rightDo))
