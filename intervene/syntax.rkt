#lang racket

;; Normalized program syntax.

(struct normReturn (vars) #:transparent)
(struct normObservation (var ovar next) #:transparent)
(struct normStatement (vars expr next) #:transparent)

(provide (struct-out normReturn))
(provide (struct-out normObservation))
(provide (struct-out normStatement))

(require do/intervene/syntax-helpers)

{require (for-template do/leftdo)}
{require (for-template do/monad/norm)}
{require (except-in (for-template racket/base) do)}


;; Printing normalized programs.

(struct normProgram (program)
  #:transparent
  #:methods gen:custom-write

  ((define (show-statements indent p)
     (match p

       [(normReturn vars)
        (format "~areturn ~a"
                (make-string indent #\space)
                (symbols->listString vars))]

       [(normObservation var ovar next)
        (format "~a() <- (observe ~a ~a)\n~a"
                (make-string indent #\space)
                (symbol->string var)
                (symbol->string ovar)
                (show-statements indent next))]

       [(normStatement vars expr next)
        (format "~a~a <- ~a\n~a"
                (make-string indent #\space)
                (symbols->listString vars)
                (show-program (+ 4 indent) expr)
                (show-statements indent next))]))

   (define (show-program indent p)
     (define program (normProgram-program p))
     (if (or (normReturn? program) (normObservation? program) (normStatement? program))
           (string-append "(do \n" (show-statements (+ 4 indent) program) ")")
           (format "~a" (syntaxSymbol->string program))))

   (define (write-proc program port mode)
     (fprintf port (show-program 0 program)))))

(provide (struct-out normProgram))
