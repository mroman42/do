#lang racket

(require do/monad)
(require do/leftdo)
(require do/struct-slide)

;; Right-sliding do-notation.
(define-syntax slideDo
  (syntax-rules (<- <~ return)

    ;; 1. Left monad multiplication.
    [(slideDo sm
           x <- f
           y <- g
           rest ...)
     (slideDo sm
           (list x y) <- (rDo (slide-monad sm)
                              x <- f
                              y <- g
                              return (list x y))
           rest ...)
     ]

    ;; 2. Monad return.
    [(slideDo sm
           x <- f
           return v)
     (rDo (slide-monad sm)
          x <- f
          return v)]
    
    ;; 3. Distribution unital.
    [(slideDo sm
           x <~ f
           rest ...)
     (slideDo sm
           '() <- ((monad-return (slide-monad sm)) '())
           x <~ f
           rest ...)]
    
    ;; 4. Distribution.
    [(slideDo sm
           u <- h
           x <~ f
           y <- g
           rest ...)
     (slideDo sm
           u <- h
           b <- ((slide-distribute sm) f (match-lambda
                    [x (rDo (slide-monad sm)
                            y <- g
                            return (list x y))]))
           (list x y) <~ b
           rest ...)]

    ;; 5. Actions still multiply.
    [(slideDo sm
           u <- h
           x <~ f
           y <~ g
           rest ...)
     (slideDo sm
           u <- h
           (list x y) <~ (rDo (slide-actor sm)
                              x <- f
                              y <- g
                              return (list x y))
           rest ...)]
    
    ;; 6. An action returns correctly.
    [(slideDo sm
           x <- f
           y <~ g
           return v)
     (rDo (slide-monad sm)
          x <- f
          y <- ((slide-morph sm) g)
          return v)]
    ))

(provide slideDo)
