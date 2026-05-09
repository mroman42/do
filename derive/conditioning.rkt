#lang racket

(require do/dag)
(require do/precondition)
(require do/syntax/do)
(require racket/set)

;; Conditionals, syntactically: this function writes the conditional P(As|Bs) in
;; terms of a distribution P(-) with variables Vs. This means
;; 
;; lDo Norm
;;     v1 ... vn <- p
;;     observe bi xi
;;     return ai
;;
;; Note how we do not return a lambda but we use the variables given.

(define (syntax-conditioning as bs vs p)

  (precondition
   (subset? (list->set as) (list->set vs))
   'conditioning "input variables ~v must be a subset of all variables ~v" as vs)

  (precondition
   (subset? (list->set bs) (list->set vs))
   'conditioning "condition variables ~v must be a subset of all variables ~v" bs vs)

  (precondition
   (set-empty? (set-intersect as bs))
   'conditioning "input ~v and condition variables ~v should be disjoint" as bs)
  
  ;; #1. Assigns a temporary variable to variables in Vs.
  ;;        x1 ... xn <- p
  ;;     V must be in vs
  (let ([vtemps (generate-temporaries vs)])

    (define (temp v)
      (define (gettemp v l)
        (match l
          ['() (error "list exhausted" vs bs)]
          [(cons (cons x y) ls) (if (equal? v x) y (gettemp v ls))]))

      (gettemp v (map cons vs vtemps)))
    
    (with-syntax
      ([(vst-s ...) vtemps]
       [(ast-s ...) (map temp as)])

      ;; #2. Yields a list of observations.
      (define (syntax-observing bs)
        (match bs
          ['() (doReturn #'Norm #'(list ast-s ...))]
          [(cons y bs)  (doStatement #'Norm
                                     #'(list) #`(observe #,y #,(temp y))
                                     (syntax-observing bs))]))

      ;; #3. Prefaces with the sampling.
      (doStatement #'Norm
                   #'(list vst-s ...) #`#,p
                   (syntax-observing bs)))))


(provide syntax-conditioning)
