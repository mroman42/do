#lang racket

(struct normReturn (vars) #:transparent)
(struct normPrimitive (var label next) #:transparent)
(struct normObserve (var ovar next) #:transparent)
(struct normExpression (var expr next) #:transparent)
(provide (struct-out normReturn))
(provide (struct-out normPrimitive))
(provide (struct-out normObserve))
(provide (struct-out normExpression))




(define (show-program indent p)
  (match p

    [(normReturn vars)
     (format "~areturn ~v" (make-string indent #\space) vars)]

    [(normPrimitive var prim next)
     (format "~a~v <- ~v\n~a" (make-string indent #\space)
                           var prim (show-program indent next))]

    [(normObserve var ovar next)
     (format "~a'() <- (observe ~v ~v)\n~a"
             (make-string indent #\space)
             var ovar (show-program indent next))]

    [(normExpression var expr next)
     (format "~a~v <- (lDo Norm\n~a)\n~a"
             (make-string indent #\space)
             var (show-program (+ 4 indent) expr) (show-program indent next))]

    ))

(define (display-program p)
  (fprintf (current-output-port) (string-append "(lDo Norm\n" (show-program 4 p) ")")))


(display-program
 (normExpression 'a
                 (normObserve 'a 'b (normPrimitive 'x 'q (normReturn '(y z))))
                 (normObserve 'x 'y
                              (normPrimitive 'x 'p
                                             (normReturn '(x y z))))))



;; (struct cake (candles)
;;   #:methods gen:custom-write
;;   [(define (write-proc cake port mode)
;;      (define n (cake-candles cake))
;;      (show "   ~a   ~n" n #\. port)
;;      (show " .-~a-. ~n" n #\| port)
;;      (show " | ~a | ~n" n #\space port)
;;      (show "---~a---~n" n #\- port))
;;    (define (show fmt n ch port)
;;      (fprintf port fmt (make-string n ch)))])

;;(cake 4)
