#lang racket

(require do/intervene/syntax)
(require do/intervene/syntax-helpers)
(require do/intervene/syntax-observations)

;; NORMCONDITIONAL
;; lDo Norm
;;     vs{temporary} <- p
;;     observe bs bs{temporary}
;;     return as
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

(define (normConditional a bs vs p)
  (define temporary-variables
    (map (lambda (x) (syntax->datum x)) (generate-temporaries vs)))

  (define (temporary v)
    (define (gettemp v l)
      (match l
        ['() (error "normConditional: list exhausted" vs bs v)]
        [(cons (cons x y) ls)
         (if (equal? v x) y (gettemp v ls))]))    
    (gettemp v (map cons vs temporary-variables)))

  (define (temporaries xs)
    (map (lambda (x) (temporary x)) xs))
  
  (normStatement temporary-variables p
                  (normObservations bs (temporaries bs)
                                    (normReturn (list (temporary a))))))

(define example1 
  (normProgram (normConditionals
                '(x y)
                '(u v w)
                '(x y u v w z)
                (normProgram #'p))))

(define example2
  (normProgram (normConditional
                'x
                '(u v w)
                '(x y u v w z)
                (normProgram #'p))))


(provide normConditional)
(provide normConditionals)
