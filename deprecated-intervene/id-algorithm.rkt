#lang racket

(require do/intervene/dag)
(require do/monad)
(require do/monad/norm)
(require do/leftdo)
(require do/intervene/syntax-identify-algorithm)
(require do/intervene/syntax)
(require do/intervene/syntax-helpers)

(require do/example/data-napkin)

(define napkin
  (Dag
   'u1 <- (list )
   'u2 <- (list )
   'w <- (list 'u1 'u2)
   'z <- (list 'w)
   'x <- (list 'z 'u1)
   'y <- (list 'x 'u2)
   visible (list 'w 'z 'x 'y)))




;; ID-ALGORITHM
(define (id-algorithm sv tv g p)

  (define (line-of v g p)
    (let* ([us (dag-c-component-until v g)]
           [qs (sce us (filter (lambda (x) (not (member x us))) (dag-visibles g)) g p)])
      (identify-algorithm (list v) us qs g)))
  
  (define (acc-id-algorithm sv tv vv g p)
    (match vv
      [(cons v vv)
       (if (member v tv)
           (acc-id-algorithm sv tv vv g p)
           (normStatement (list v)
                          (normProgram (line-of v g p))
                          (acc-id-algorithm sv tv vv g p)))]
      ['()  (normReturn sv)]))

  (acc-id-algorithm sv tv (dag-visibles g) g p))


;; Algorithm examples.
(define example-napkin
  (normProgram
   (id-algorithm '(y) '(x) napkin (normProgram #'p))))

(define example2
  (normProgram
   (id-algorithm '(cancer) '(smoking) (Dag
                            'genes <- (list)
                            'smoking <- (list 'genes)
                            'tar <- (list 'smoking)
                            'cancer <- (list 'tar 'genes)
                            visible (list 'smoking 'tar 'cancer)) (normProgram #'p))))

(provide id-algorithm)
