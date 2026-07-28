#lang racket

(require do/monad/norm)
(require do/leftdo)
(require do/intervene/syntax)

{require (for-syntax racket/base)}
{require (for-syntax racket/list)}
{require (for-syntax do/intervene/syntax)}
{require (for-syntax do/intervene/id-algorithm)}
{require (for-syntax do/intervene/dag)}
{require (for-syntax do/leftdo)}

{require racket/base}
{require racket/list}
{require do/intervene/syntax}
{require do/intervene/id-algorithm}
{require do/intervene/dag}
{require do/leftdo}


(require do/example/napkin-problem)



(define-syntax (Identify2 stx)
  (syntax-case stx ()
    [(_ (y ...) _ (x ...) _ p _ g)
     (with-syntax ([stx-transformed
                    (normReifyWithLambda (syntax->datum #'(x ...))
                               (normProgram
                                (id-algorithm
                                 (syntax->datum #'(y ...))
                                 (syntax->datum #'(x ...))
                                 (dagParse #'g)
                                 #'p)))])
       #'stx-transformed)]))

(define-syntax (Identify stx)
  (syntax-case stx ()
    [(_ (y ...) _ (x ...) _ (i ...) _ g _ p)
     (with-syntax ([stx-transformed
                    #`(#,(normReifyWithLambda (syntax->datum #'(x ...))
                               (normProgram
                                (id-algorithm
                                 (syntax->datum #'(y ...))
                                 (syntax->datum #'(x ...))
                                 (dagParse #'g)
                                 #'p))) i ...)])
       #'stx-transformed)]))

(define (IdentifySyntax stx)
  (syntax-case stx ()
    [(_ (y ...) _ (x ...) _ (i ...) _ g _ p)
     (with-syntax ([stx-transformed
                    #`(#,(normReifyWithLambda (syntax->datum #'(x ...))
                               (normProgram
                                (id-algorithm
                                 (syntax->datum #'(y ...))
                                 (syntax->datum #'(x ...))
                                 (dagParse #'g)
                                 #'p))) i ...)])
       #'stx-transformed)]))


(define-syntax (Intervene stx)
  (syntax-case stx ()
    [(_ p _ g _ (x ...) (y ...) _ (i ...) )
     (with-syntax ([stx-transformed
                    #`(#,(normReifyWithLambda (syntax->datum #'(x ...))
                               (normProgram
                                (id-algorithm
                                 (syntax->datum (dagParseVisibles #'g))
                                 (syntax->datum #'(x ...))
                                 (dagParse #'g)
                                 #'p))) i ...)])
       #'stx-transformed)]))

(provide Identify)
(provide IdentifySyntax)
(provide Identify2)
(provide Intervene)
(provide (all-from-out do/monad/norm))
(provide (all-from-out do/leftdo))
