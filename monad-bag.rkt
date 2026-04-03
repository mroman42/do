#lang racket

(require leftdo/monad)
(require leftdo/leftdo)
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
  (monad bag-return bag-bind bag-map))

(define bag-join (monad-join Bag))

(define (bag->list b) (multiset->list b))

(define bag multiset)

(provide Bag
         bag-map
         bag-bind
         bag-return
         bag-join
         bag->list
         bag
         multiset
         )

