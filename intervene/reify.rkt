#lang racket

(require do/intervene/syntax)
(require do/intervene/syntax-helpers)
{require (for-syntax racket/base)}
{require (for-template do/notation/normDo)}
{require (for-template (except-in racket/base do))}

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
    ([(items ...) (map (lambda (v) (temporary-name-stx v)) vs)])
    #'(items ...)))

(define (temporary-vars vs)
  (with-syntax
    ([(items ...) (map (lambda (v) (temporary-name-stx v)) vs)])
    #'(items ...)))

(define (temporary-vars-list vs)
  (with-syntax
    ([(items ...) (map (lambda (v) (temporary-name-stx v)) vs)])
    #'(list items ...)))


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
           (append (list #'do) (normReifyList s))
           (list s))]
       [(normReturn vs)
        (if (list? vs)
            (list #'return (vars-to-syntax vs))
            (list #'return (temporary-name-stx vs)))]
       [(normObservation x y next)
        (append (list #'() #'<- #`(observe #,(temporary-name-stx x) #,(temporary-name-stx y)))
                (normReifyList next))]
       [(normStatement x e next)
        (if (normProgram? e)
            (match e
              [(normProgram e)
               (if (or (normReturn? e)
                       (normObservation? e)
                       (normStatement? e))
                   (append (list (temporary-case x) #'<- (normReify (normProgram e))) (normReifyList next))
                   (append (list (temporary-case x) #'<- e) (normReifyList next)))]
              [e (append (list (temporary-case x) #'<- e) (normReifyList next))])
            (append (list (temporary-case x) #'<- e) (normReifyList next)))]
       [p (list p)]))
  (with-syntax ([(items ...) (normReifyList s)])
     #'(items ...)))


(define (normReifyWithLambda xs s)
  #`(lambda #,(temporary-vars xs) #,(normReify s)))



(define (withBinding x y p)
  (with-syntax ([(i ...) y]
                [(j ...) x])
    #`((lambda (j ...) #,p) i ...)))

(define (normReifyWithBinding xs is s)
  (withBinding (temporary-vars xs) is (normReify s)))

(provide normReify)
(provide normReifyWithLambda)
(provide normReifyWithBinding)
(provide withBinding)
(provide temporary-vars)
(provide temporary-vars-list)
