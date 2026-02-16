#lang racket

(require racket/struct)
(require leftdo/monad)
(require leftdo/left-do)
(require rackunit)
(require leftdo/normalization-almost-monad)
(require leftdo/normalized-distributions)
(require rebellion/collection/multiset)



(define (bag-map f mx)
  (for/multiset
      ([x (multiset->list mx)])
    (f x)))

(define (bag-bind mx f)
  (apply multiset
    (append-map (lambda (m) (multiset->list m))
         (map f (multiset->list mx)))))

(define (bag-return x)
  (multiset x))

(define Bag
  (monad
   bag-return
   bag-bind
   bag-map))

(define bag-join (monad-join Bag))


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
  (r-seq (multiset->list b)))

(define (lbag-seq b)
  (dist-map (lambda (x) (apply multiset x)) (l-seq (multiset->list b))))


(define (normbag-return x)
  (norm-return (multiset x)))

(define (normbag-map f mx)
  (norm-map (lambda (my) (bag-map f my)) mx))

(define (normbag-bind mx f)
   (norm-join
     (norm-map (lambda (nbbx) (norm-map (lambda (bbx) (bag-join bbx)) nbbx))
               (norm-map (lambda (m) (lbag-seq m))
                         (normbag-map f mx)))))


(define NormBag
  (monad
    normbag-return
    normbag-bind
    normbag-map))


(define (frequentist nbx)
  (norm-join (norm-map (lambda (bx) (uniform (multiset->list bx))) nbx)))

(define-syntax distribution
  (syntax-rules ()
    [(_ [x v] rest ...)  (cons (list (multiset x) v) (distribution rest ...))]
    [(_)                 (list)]))

(define-syntax bag
  (syntax-rules ()
    [(_ x ...)  (norm-return (multiset x ...))]
    ))


(define (observe x y)
  (if (equal? x y)
      (uniform (list (bag-return '())))
      (uniform (list))))


(provide distribution
         bag)
(provide NormBag)
(provide normbag-bind normbag-return normbag-map)
(provide observe
         frequentist)