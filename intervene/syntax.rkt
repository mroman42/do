#lang racket

(struct normReturn (vars) #:transparent)
(struct normObservation (var ovar next) #:transparent)
(struct normStatement (vars expr next) #:transparent)

(provide (struct-out normReturn))
(provide (struct-out normObservation))
(provide (struct-out normStatement))

(require do/intervene/syntax-helpers)
(require do/leftdo)
{require (for-syntax do/leftdo)}
{require (for-syntax racket/base)}
{require (for-syntax racket/list)}
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

(define (vars-to-syntax LIST vs)
  (with-syntax
    ([(items ...) (cons LIST (map (lambda (v) (temporary-name v)) vs))])
    #'(items ...)))

(define (temporary-case LIST vs)
  (if (list? vs)
      (vars-to-syntax LIST vs)
      (temporary-name vs)))

(define (normReify lDo Norm LIST s)
   (define (normReifyList s)
     (match s
       [(normProgram s)
          (if (or (normReturn? s)
                  (normObservation? s)
                  (normStatement? s))
           (append (list lDo Norm) (normReifyList s))
           (list s))]
       [(normReturn vs)
        (if (list? vs)
            (list #'return (vars-to-syntax LIST vs))
            (list #'return (temporary-name vs)))]
       [(normObservation x y next)
        (append (list (temporary-case LIST 'obs) #'<- #`(observe #,(temporary-name x) #,(temporary-name y)))
                (normReifyList next))]
       [(normStatement x e next)
        (if (normProgram? e)
            (append (list (temporary-case LIST x) #'<- (normReify lDo Norm LIST e))  
                    (normReifyList next))
            (append (list (temporary-case LIST x) #'<- e)
                    (normReifyList next)))]
       [p (list p)]))
  (with-syntax ([(items ...) (normReifyList s)])
     #'(items ...)))


(provide normReify)

(define (example p)
  (normReify #'lDo #'Norm #'list (normProgram
     (normStatement 'x (normProgram
                        (normStatement 'x p (normReturn 'x)))
                    (normStatement 'y (normProgram (normStatement 'z (normProgram (normReturn 'x)) (normReturn 'z)))
                                   (normObservation 'x 'y (normReturn '(y))))))))

(provide example)
