#lang racket

(require leftdo/monad)
(require leftdo/action)
(require leftdo/leftdo)

(define-syntax actAccDo
  (syntax-rules (<- <~ return)

    ;; If you find an action, you are done: evaluate the rest.
    [(actAccDo am acc accVar
            var <~ aexp
            rest ...)
     (rDo (action-monad am)
          accVar <- acc
          value <- ((action-act am) aexp
                                    (match-lambda [var (actDo am rest ...)]))
          return value)
     ]
    
    ;; On normal days, left-fold.
    [(actAccDo am acc accVar
            var <- mexp
            rest ...)
     (actAccDo am
            (rDo (action-monad am)
                 accVar <- acc
                 var <- mexp
                 return (list var accVar))
            (list var accVar)
            rest ...)
     ]

    ;; Finally, return.
    [(actAccDo am acc accVar
            return value)
     (rDo (action-monad am)
          accVar <- acc
          return value)
     ]))


(define-syntax actDo
  (syntax-rules (<- <~ return)
    [(actDo am rest ...)
     (actAccDo am (rDo (action-monad am)
                       return (list)) (list) rest ...)]))


(provide actDo)
