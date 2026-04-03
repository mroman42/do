#lang racket

(require leftdo/monad)
(require leftdo/leftdo)
(require leftdo/monad-norm)
(require leftdo/monad-bag)




(define/match (r-seq l)
  [('()) (norm-return '())]
  [((cons mx mxs))
   (norm-bind mx (λ (x)
      (norm-bind (r-seq mxs) (λ (l)
         (norm-return (cons x l))))))])

(define/match (l-seq l)
  [('()) (norm-return '())]
  [(mxs)
   (rDo Norm
        xs <- (l-seq (drop-right mxs 1))
        x  <- (last mxs)
        return (append xs (list x)))])

(define (rbag-seq b)
  (r-seq (bag->list b)))

(define (lbag-seq b)
  (dist-map (lambda (x) (apply multiset x)) (l-seq (bag->list b))))


(define (normbag-return x)
  (norm-return (multiset x)))

(define (normbag-map f mx)
  (norm-map (lambda (my) (bag-map f my)) mx))

(define (normbag-bind mx f)
   (norm-join
     (norm-map
       (lambda (nbbx) (norm-map (lambda (bbx) (bag-join bbx)) nbbx))
       (norm-map (lambda (m) (lbag-seq m)) (normbag-map f mx)))))


(define NormBag
  (monad
    normbag-return
    normbag-bind
    normbag-map))


(define (frequentist nbx)
  (norm-join (norm-map (lambda (bx) (dist-uniform (bag->list bx))) nbx)))

(define-syntax distribution
  (syntax-rules ()
    [(_ [x v] rest ...)  (cons (list (bag-return x) v) (distribution rest ...))]
    [(_)                 (list)]))

(define-syntax bag
  (syntax-rules ()
    [(_ x ...)  (norm-return (multiset x ...))]
    ))


(define (observe x y)
  (if (equal? x y)
      (uniform (bag-return '()))
      (uniform)))


(provide distribution bag)
(provide NormBag
         normbag-bind
         normbag-return
         normbag-map
         observe
         frequentist)
