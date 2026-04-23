#lang racket

{begin-for-syntax

  (require racket/match)
  (require syntax/parse)
  
  (struct doReturn (monad vars) #:transparent)
  (struct doStatement (monad var expr next) #:transparent)

  ;; 1. Parsing the syntax into a structure.
  (define (doParse stx)
    (syntax-case stx ()
      [(_ m return v)  (doReturn #'m #'v)]
      [(_ m x <- e rest ...)
       (doStatement #'m #'x #'e
                    (doParse #'(do m rest ...)))]))

  ;; 2. Reversing the structure.
  (define (reverseDo s)
    (define (getReturn s)
      (match s
        [(doReturn m v) (doReturn m v)]
        [(doStatement m x e r) (getReturn r)]))

    (define (reverseDoAcc s acc)
      (match s
        [(doReturn m v) acc]
        [(doStatement m x e r) (reverseDoAcc r (doStatement m x e acc))]))

    (reverseDoAcc s (getReturn s)))

  ;; 3. Reifying the syntax.
  (define (doReify s)
    (define (doReifyList s)
      (match s
        [(doReturn m v) (list #'return v)]
        [(doStatement m x e r)
         (append (list x #'<- e) (doReifyList r))]))

    (define (doReifiedList s)
      (append (list #'lDo #'Norm) (doReifyList s)))

    (with-syntax ([(items ...) (doReifiedList s)])
      #'(items ...)))

  ;(displayln (syntax->datum (doReify (doParse #'(lDo Norm x <- (uniform 3) return x)))))
  }

;; 4. Macro, with syntax.
{define-syntax (reversedDo stx)
  (with-syntax ([stx-transformed (doReify (reverseDo (doParse stx)))]) #'stx-transformed)}

(require leftdo/leftdo)
(require leftdo/leftdo-left)
(require leftdo/monad)
(require leftdo/monad-norm)

(lDo Norm
     y <- (uniform 'x 'y)
     x <- (uniform y)
     return (list x y))

(reversedDo Norm
  x <- (uniform y)
  y <- (uniform 'x 'y)
  return (list x y))
