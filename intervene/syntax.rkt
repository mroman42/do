#lang racket

(struct normReturn (vars) #:transparent)
(struct normObservation (var ovar next) #:transparent)
(struct normStatement (vars expr next) #:transparent)

(provide (struct-out normReturn))
(provide (struct-out normObservation))
(provide (struct-out normStatement))



(struct normProgram (program)
  #:transparent
  #:methods gen:custom-write
  ((define (show-statements indent p)
     (match p

       [(normReturn vars)
        (format "~areturn ~v" (make-string indent #\space) vars)]

       [(normObservation var ovar next)
        (format "~a'() <- (observe ~v ~v)\n~a"
                (make-string indent #\space)
                var ovar (show-statements indent next))]

       [(normStatement vars expr next)
        (format "~a~v <- ~a\n~a"
                (make-string indent #\space)
                vars (show-program (+ 4 indent) expr) (show-statements indent next))]))

   (define (show-program indent p)
     (define program (normProgram-program p))
     (if (or (normReturn? program) (normObservation? program) (normStatement? program))
           (string-append "(lDo Norm\n" (show-statements (+ 4 indent) program) ")")
           (format "~v" program)))

   (define (write-proc program port mode)
     (fprintf port (show-program 0 program)))))

; EXAMPLES
;; (normProgram
;;  (normStatement 'x (normProgram (normReturn 'x))
;;                 (normStatement 'y (normProgram (normStatement 'z (normProgram (normReturn 'x)) (normReturn 'z)))
;;                 (normObservation 'x 'y (normReturn '(y))))))
;(normProgram (normReturn '(y)))
;(normProgram 'p)

(provide (struct-out normProgram))
