#lang racket

(struct normReturn (vars) #:transparent)
(struct normPrimitive (var label next) #:transparent)
(struct normObservation (var ovar next) #:transparent)
(struct normExpression (vars expr next) #:transparent)
(provide (struct-out normReturn))
(provide (struct-out normPrimitive))
(provide (struct-out normObservation))
(provide (struct-out normExpression))



(define (show-program indent p)
  (match p

    [(normReturn vars)
     (format "~areturn ~v" (make-string indent #\space) vars)]

    [(normPrimitive var prim next)
     (format "~a~v <- ~v\n~a" (make-string indent #\space)
                           var prim (show-program indent next))]

    [(normObservation var ovar next)
     (format "~a'() <- (observe ~v ~v)\n~a"
             (make-string indent #\space)
             var ovar (show-program indent next))]

    [(normExpression vars expr next)
     (format "~a~v <- (lDo Norm\n~a)\n~a"
             (make-string indent #\space)
             vars (show-program (+ 4 indent) expr) (show-program indent next))]

    ))

(define (display-program p)
  (fprintf (current-output-port) (string-append "(lDo Norm\n" (show-program 4 p) ")")))



(provide display-program)
