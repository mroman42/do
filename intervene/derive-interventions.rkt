#lang racket

(require do/notation/leftDo)
(require do/intervene/algorithm-id)
(require do/intervene/dag)
(require (for-syntax do/intervene/algorithm-id))
(require (for-syntax do/intervene/syntax))
(require (for-syntax do/intervene/dag))
(require (for-syntax do/intervene/reify))
(require (for-syntax do/intervene/simplify-unitality))


(define-syntax (interveneStx stx)
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

(define-syntax (Intervene2 stx)
  (syntax-case stx ()
    [(_ p _ g _ (x ...) _ (i ...) _ (y ...))
     (with-syntax
       ([stx-transformed
           #`(#,(normReify (algorithm-id
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
                             ;; why this? (syntax->datum #'(i ...))
                                      (normReify (algorithm-id
                                       (normProgram #'p)
                                       (dagParse #'g)
                                       (syntax->datum #'(x ...))
                                       (syntax->datum #'(y ...)))))])
            (first (first p)))])
       #'stx-transformed)]))

;; (define-syntax (InterveneT stx)
;;   (syntax-case stx ()
;;     [(_ p _ g _ (x ...) _ (i ...) _ (y ...))
;;      (with-syntax
;;        ([stx-transformed
;;               (withBinding (temporary-vars (syntax->datum #'(x ...))) (syntax->datum #'(i ...))
;;                                       (normReify (algorithm-id
;;                                        (normProgram #'p)
;;                                        (dagParse #'g)
;;                                        (syntax->datum #'(x ...))
;;                                        (syntax->datum #'(y ...)))))])
;;        #'stx-transformed)]))

(provide interveneStx)
(provide intervene)
