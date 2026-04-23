#lang racket

(require leftdo/monad)

;(define (resolve observableVariables dependencyList))

(define (member? item seq)
  (sequence-ormap (lambda (x)
                    (equal? item x))
                  seq))


(define (rewrite observable dependencies)
  (match dependencies
    [(list)                 (list)]
    [(cons (list x a) rest)
     (if (member? x observable)
         (cons (list x a) (rewrite observable rest))
         (list (list x a))
         )]))

(struct dah-statement (var inputs next))
(struct dah-visible (visibles))


(define-syntax Dah
  (syntax-rules (<== visible)
    [(_ (x <== xds ...) more ...)  (dah-statement x (list xds ...) (Dah more ...))]
    [(_ (visible vs ...))          (dah-visible (vs ...))]
    ))

(Dah
  ('x <== 'a 'b 'c)
  (visible 'x))

;(Dah
;  ('x <== 'a 'b)
;  ('y <== 'a 'x)
;  (visible 'x 'y))



;(define (print-identification deps)
;  (for ([item deps])
;    (match item
;      [(list x acs) (printf "~a <== ~a\n" x acs)]
;      []
;      )

