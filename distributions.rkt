#lang racket


(require do/monad)
(require (for-syntax racket/syntax syntax/parse))

;; This parameterized macro module implements distributions and the normalized
;; distribution monad over a positive semiring.


(define R0 0)
(define R1 1)
(define R+ +)
(define R* *)
(define Rninv? zero?)
(define R/ /)

(define (pair x y) (list x y))

(define (validity xs)
  (apply R+ (map second xs)))

(define/match (dist-map f xs)
  [(f  '())                  null]
  [(f  (cons (list x v) ys)) (cons (list (f x) v) (dist-map f ys))])

;; But we can also map on the values.
(define/match (dist-map-values f xs)
  [(f  '())                  null]
  [(f  (cons (list x v) ys)) (cons (list x (f v)) (dist-map-values f ys))])

;; Remove zeroes
(define/match (remove-zeroes xs)
  [('())  null]
  [((cons (list x v) ys))
   (if (equal? v R0)
       (remove-zeroes ys)
       (cons (list x v) (remove-zeroes ys))
       )])

;; Weight of a single point of a distribution.
(define/match (weight-of-point x xs)
  [(x '()) R0]
  [(x (cons (list x v) ys)) (R+ v (weight-of-point x ys))]
  [(x (cons (list y v) ys)) (weight-of-point x ys)])

;; Reweighting a distribution.
(define/match (dist-remove x xs)
  [(x '())  '()]
  [(x (cons (list x v) xs))  (dist-remove x xs)]
  [(x (cons (list y v) xs))  (cons (list y v) (dist-remove x xs))])

(define/match (reweight xs)
  [('()) '()]
  [((cons (list x v) ys))
   (let ([w   (R+ v (weight-of-point x ys))])
     (cons (list x w) (reweight (dist-remove x ys))))])

(define (from-table l)
  (dist-map-values (lambda (v) (R/ v (validity l))) l))




;; Condensing a distribution into a valid distribution.
(define (condense xs)
  (reweight (remove-zeroes xs)))

;; Subdistributions of subdistributions.
(define/match (rescale xss)
  [((list xs v))  (dist-map-values (lambda (x) (R* v x)) xs)])

(define (dist-join xss)
  (condense (apply append (map rescale xss))))

(define (dist-normalize xs)
  (condense (dist-map-values (lambda (v) (R/ v (validity xs))) xs)))

(define (dist-bind xs f)
  (dist-join (dist-map f xs)))

(define (dist-return x)
  (list (pair x R1)))

(define (dist-length ls)
  (apply R+ (map (lambda (x) R1) ls)))

(define (dist-uniform ls)
  (map (lambda (x) (pair x (R/ R1 (dist-length ls)))) ls))


(define (dist-coin p)
  (list
   (pair #t p)
   (pair #f (- 1 p))))

(define dist-void (list))

(define-syntax distribution
  (syntax-rules ()
    [(_ [x v] rest ...)  (cons (pair x v) (distribution rest ...))]
    [(_)           (list)]))

(define-syntax uniform
  (syntax-rules ()
    [(_ x ...) (dist-uniform (list x ...))]))

(define-syntax distribution-table
  (syntax-rules ()
    [(_ rest ...)  (from-table (distribution rest ...))]))

(define-syntax define/table
  (syntax-rules ()
    [(_ name (x ...)) (define name (distribution-table x ...))]))


(define norm-return dist-return)

(define (norm-bind xs f)
  (dist-normalize (dist-bind xs f)))

(define (norm-map f xs)
  (dist-normalize (dist-map f xs)))

(define Norm
  (monad
   norm-return
   norm-bind
   norm-map))

(define (observe x y)
  (if (equal? x y)
      (uniform '())
      (uniform)))

(provide dist-uniform
         dist-map
         distribution
         Norm
         norm-bind
         observe
         uniform
         define/table)

