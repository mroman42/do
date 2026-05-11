#lang racket

(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/syntax-conditional)
(require do/precondition)
(require do/intervene/syntax-identify-algorithm)

(define (select x p)
  (match p
    [(normReturn vars)               (normReturn x)]
    [(normObservation var ovar more) (error "unexpected!")]
    [(normStatement y q more)
     (if (equalDatum? y x)
         (normStatement y q (normReturn y))
         (normStatement y q (select x more)))]))

(define (select-program x p)
  (normProgram (select x (normProgram-program p))))

(provide select)
(provide select-program)

;; (select-program #'y
;;    (sce '(y) '(x)
;;                (Dag
;;                  'u1 <- '()
;;                  'u2 <- '()
;;                  'w <- '(u1 u2)
;;                 'x <- '(w u1)
;;                 'z <- '(x)
;;                 'y <- '(z u2)
;;                 visible '(w x z y))
;;               (normProgram 'p)))
