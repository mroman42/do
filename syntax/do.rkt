#lang racket

(struct doReturn (monad vars) #:transparent)
(struct doStatement (monad var expr next) #:transparent)

(provide (struct-out doReturn))
(provide (struct-out doStatement))  

;; 1. Parsing the syntax into a structure.
(define (doParse stx)
  (syntax-case stx ()
    [(_ m return v)  (doReturn #'m #'v)]
    [(_ m x <- e rest ...)
     (doStatement #'m #'x #'e
                  (doParse #'(do m rest ...)))]))

(provide doParse)


 ;; 2. Reversing the structure.
 (define (doReverse s)
   (define (getReturn s)
     (match s
       [(doReturn m v) (doReturn m v)]
       [(doStatement m x e r) (getReturn r)]))

   (define (doReverseAcc s acc)
     (match s
       [(doReturn m v) acc]
       [(doStatement m x e r) (doReverseAcc r (doStatement m x e acc))]))

   (doReverseAcc s (getReturn s)))

(provide doReverse)

(define (doReify d s)
   (define (doReifyList s)
     (match s
       [(doReturn m v) (list #'return v)]
       [(doStatement m x e r)
        (append (list x #'<- e) (doReifyList r))]))

   (define (doReifiedList s)
     (match s
       [(doReturn m v)  (append (list d m) (doReifyList s))]
       [(doStatement m x e r) (append (list d m) (doReifyList s))]))

   (with-syntax ([(items ...) (doReifiedList s)])
     #'(items ...)))

(provide doReify)


;(displayln (syntax->datum (doReify (doParse #'(lDo Norm x <- (uniform 3) return x)))))


;(provide doParse)
;(provide reverseDo)
;(provide doReify)
