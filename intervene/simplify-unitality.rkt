#lang racket

(require do/intervene/syntax)

#| SIMPLIFY-UNITALITY

Simplifies a syntactic program recursively using the unitality axiom of a unital
magmad. |#

;; (define (simplify-unitality p)
;;   (define (go p)
;;     (match p

;;       [(normStatement X (normProgram F) (normReturn Y))
;;        (if (equal? X Y)
;;            (go F)
;;            (normStatement X (normProgram (go F)) (normReturn Y)))]

;;       [(normStatement X F more)
;;        (normStatement X (simplify-unitality F) (go more))]
      
;;       [(normObservation x y more)
;;        (normObservation x y (go more))]

;;       [(normReturn Y) (normReturn Y)]

;;       [q q]))

;;   (match p
;;     [(normProgram p) (normProgram (go p))]))

(define (simplify-unitality p)
  (define (go p)
    (match p
      [(normStatement X F more)
       (normStatement X (simplify-unitality F) (go more))]
      [(normObservation x y more)
       (normObservation x y (go more))]
      [(normReturn Y)
       (normReturn Y)]
      [q q]))
  
  (match p
    [(normProgram (normStatement X F (normReturn Y)))
     (if (equal? X Y)
         (simplify-unitality F)
         (normProgram (normStatement X (simplify-unitality F) (normReturn Y))))]
    [(normProgram q) (normProgram (go q))]))


;; (define (simplify-unitality p)
;;   )

;;     [(normProgram (normStatement X (normProgram F) more))
;;      (normProgram X (simplify-unitality F) (go more))

;;       [(normStatement X F more)
;;        (normStatement X (simplify-unitality F) (go more))]
      
;;       [(normObservation x y more)
;;        (normObservation x y (go more))]

;;       [(normReturn Y) (normReturn Y)]

;;       [q q]))

;;   (match p
;;     [(normProgram p) (normProgram (go p))]))

(provide simplify-unitality)
