#lang racket

(require do/intervene/syntax)
(require do/intervene/syntax-observations)

;; NORMCONDITIONAL
(define (normConditional as bs vs p)
  
  (define temporary-variables
    (map (lambda (x) (syntax->datum x)) (generate-temporaries vs)))

  (define (temporary v)
    (define (gettemp v l)
      (match l
        ['() (error "list exhausted" vs bs)]
        [(cons (cons x y) ls) (if (equal? v x) y (gettemp v ls))]))    
    (gettemp v (map cons vs temporary-variables)))

  (define (temporaries xs)
    (map (lambda (x) (temporary x)) xs))
  
  (normExpression temporary-variables p
                  (normObservations bs (temporaries bs)
                                    (normReturn as))))


(provide normConditional)
