#lang racket

(struct normReturn (vars) #:transparent)
(struct normObservation (var ovar next) #:transparent)
(struct normStatement (vars expr next) #:transparent)

(provide (struct-out normReturn))
(provide (struct-out normObservation))
(provide (struct-out normStatement))

(require do/intervene/syntax-helpers)

;{require (for-syntax do/leftdo)}
;{require (for-syntax racket/base)}
;{require (for-syntax racket/list)}
{require (for-template do/leftdo)}
{require (for-template do/monad/norm)}
{require (for-template racket/base)}

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
        (format "~a'() <- (observe ~a ~a)\n~a"
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
           (string-append "(lDo Norm\n" (show-statements (+ 4 indent) program) ")")
           (format "~a" (syntaxSymbol->string program))))

   (define (write-proc program port mode)
     (fprintf port (show-program 0 program)))))

(provide (struct-out normProgram))

; EXAMPLES
;; (normProgram
;;  (normStatement 'x (normProgram (normReturn 'x))
;;                 (normStatement 'y (normProgram (normStatement 'z (normProgram (normReturn 'x)) (normReturn 'z)))
;;                 (normObservation 'x 'y (normReturn '(y))))))
;(normProgram (normReturn '(y)))
;(normProgram 'p)

(require do/leftdo)
(require do/monad/norm)

(define (memoize f)
  (let ([cache (make-hash)])
    (lambda (arg)
      (hash-ref! cache arg (lambda () (f arg))))))

(define temporary-name
  (memoize (lambda (x)
             (match (generate-temporaries (list x))
               [(list x) (syntax->datum x)]))))

(define temporary-name-stx
  (memoize (lambda (x)
             (match (generate-temporaries (list x))
               [(list x) x]))))

(define (vars-to-syntax vs)
  (with-syntax
    ([(items ...) (cons #'list (map (lambda (v) (temporary-name-stx v)) vs))])
    #'(items ...)))

(define (temporary-vars vs)
  (with-syntax
    ([(items ...) (map (lambda (v) (temporary-name-stx v)) vs)])
    #'(items ...)))


(define (temporary-case vs)
  (if (list? vs)
      (vars-to-syntax vs)
      (temporary-name-stx vs)))

(define (normReify s)
   (define (normReifyList s)
     (match s
       [(normProgram s)
          (if (or (normReturn? s)
                  (normObservation? s)
                  (normStatement? s))
           (append (list #'lDo #'Norm) (normReifyList s))
           (list s))]
       [(normReturn vs)
        (if (list? vs)
            (list #'return (vars-to-syntax vs))
            (list #'return (temporary-name-stx vs)))]
       [(normObservation x y next)
        (append (list (temporary-case 'obs) #'<- #`(observe #,(temporary-name-stx x) #,(temporary-name-stx y)))
                (normReifyList next))]
       [(normStatement x e next)
        (if (normProgram? e)
            (append (list (temporary-case x) #'<- (normReify e))  
                    (normReifyList next))
            (append (list (temporary-case x) #'<- e)
                    (normReifyList next)))]
       [p (list p)]))
  (with-syntax ([(items ...) (normReifyList s)])
     #'(items ...)))

(define (normReifyWithLambda xs s)
  #`(lambda #,(temporary-vars xs) #,(normReify s)))

(provide normReify)
(provide normReifyWithLambda)

(define (example p)
  (normReify (normProgram
     (normStatement 'x p (normReturn 'x)))))

(define (example2 p)
  (normReifyWithLambda (syntax->datum #'(y)) (normProgram
     (normStatement 'x p (normReturn 'y)))))


(provide example)
