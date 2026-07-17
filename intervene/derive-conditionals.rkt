#lang racket

(require do/intervene/syntax)
(require do/intervene/syntax-helpers)
(require do/intervene/derive-observations)


;; NORMCONDITIONAL
;; (do
;;     vs{temporary} <- p
;;     observe bs bs{temporary}
;;     return as)

(define (normConditionals as bs vs p)

  (define temporary-variables
    (map (lambda (x) (syntax->datum x)) (generate-temporaries vs)))

  (define (temporary v)
    (define (gettemp v l)
      (match l
        ['() (error "normConditionals: list exhausted" vs bs)]
        [(cons (cons x y) ls)
         (if (equal? v x) y (gettemp v ls))]))    
    (gettemp v (map cons vs temporary-variables)))

  (define (temporaries xs)
    (map (lambda (x) (temporary x)) xs))
  
  (normStatement temporary-variables p
  (normObservations bs (temporaries bs)
  (normReturn (temporaries as)))))

(provide normConditionals)
