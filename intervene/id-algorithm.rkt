#lang racket

(require do/intervene/dag)
(require do/monad)
(require do/monad/norm)
(require do/leftdo)
(require do/intervene/syntax-identify-algorithm)
(require do/intervene/syntax)

(define napkin
  (Dag
   'u1 <- '()
   'u2 <- '()
   'w <- '(u1 u2)
   'z <- '(w)
   'x <- '(z u1)
   'y <- '(x u2)
   visible '(w z x y)))


(define (line-of v g p)
  (let* ([us (dag-c-component-of v g)]
         [qs (sce us (filter (lambda (x) (not (member x us))) (dag-visibles g)) g p)])
    (identify-algorithm (list v) us qs g)))

;; ID-ALGORITHM
(define (id-algorithm sv tv g p)
  
  (define (acc-id-algorithm sv tv vv g p)
    (match vv
      [(cons v vv)  (if (member v tv)
                        (acc-id-algorithm sv tv vv g p)
                        (normStatement v (normProgram (line-of v g p)) (acc-id-algorithm sv tv vv g p)))]
      ['()  (normReturn sv)]))

  (acc-id-algorithm sv tv (dag-visibles g) g p))

;(normProgram (line-of 'z napkin (normProgram 'p)))
;(normProgram (id-algorithm '(y) '(x) napkin (normProgram 'p)))
