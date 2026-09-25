#lang racket

(require do/intervene/algorithm-id)
(require do/intervene/dag)
(require (for-syntax do/intervene/algorithm-id))
(require (for-syntax do/intervene/syntax))
(require (for-syntax do/intervene/dag))
(require (for-syntax do/intervene/simplify-unitality))
(require (for-syntax do/intervene/reify))



(define-syntax (intervene-showSyntax stx)
  (syntax-case stx ()
    [(_ p _ g _ (x ...) _ (i ...) _ (y ...))
     (with-syntax
       ([stx-transformed
           #`(display #,(simplify-unitality (algorithm-id
                 (normProgram #'p)
                 (dagParse #'g)
                 (syntax->datum #'(x ...))
                 (syntax->datum #'(y ...)))))])
       #'stx-transformed)]))


(define-syntax (intervene stx)
  (syntax-case stx ()
    [(_ p _ g _ (x ...) _ (i ...) _ (y ...))
     (with-syntax
       ([stx-transformed
         #`((match-lambda [#,(temporary-vars-list (syntax->datum (dagParseVisibles #'g)))
              #,(withBinding (temporary-vars (syntax->datum #'(x ...))) #'(i ...)
                             (normReify (algorithm-id
                                         (normProgram #'p)
                                         (dagParse #'g)
                                         (syntax->datum #'(x ...))
                                         (syntax->datum #'(y ...)))))])
            (first (first p)))])
       #'stx-transformed)]))



(provide intervene-showSyntax)
(provide intervene)
