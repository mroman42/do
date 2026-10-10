#lang racket

(require "monad.rkt")

#| NORMALIZED DISTRIBUTIONS.

This file implements normalized distributions and the normalized distribution
magmad using the standard encoding of distributions as formal sums.
|#

(define (pair x y)
  (list x y))


;; FORMAL SUMS.
;; Formal sums form a functor: we can map over them.
(define/match (formal-map f xs)
  [(f  '())                  null]
  [(f  (cons (list x v) ys)) (cons (list (f x) v) (formal-map f ys))])

;; Formal sums also allow mapping over the coefficients.
(define/match (map-on-values f xs)
  [(f  '())                  null]
  [(f  (cons (list x v) ys)) (cons (list x (f v)) (map-on-values f ys))])

;; The validity of a formal sum is the sum of the coefficients.
(define (validity xs)
  (apply + (map second xs)))

;; Remove zeroes from a formal sum.
(define/match (remove-zeroes xs)
  [('())  null]
  [((cons (list x v) ys))
   (if (equal? v 0)
       (remove-zeroes ys)
       (cons (list x v) (remove-zeroes ys)))])

;; Coefficient of an element on a formal sum.
(define/match (weight-of-point x xs)
  [(x '()) 0]
  [(x (cons (list x v) ys)) (+ v (weight-of-point x ys))]
  [(x (cons (list y v) ys)) (weight-of-point x ys)])

;; Remove an element from a formal sum.
(define/match (dist-remove x xs)
  [(x '())  '()]
  [(x (cons (list x v) xs))  (dist-remove x xs)]
  [(x (cons (list y v) xs))  (cons (list y v) (dist-remove x xs))])

;; Merge the coefficients of a formal sum.
(define/match (reweight xs)
  [('()) '()]
  [((cons (list x v) ys))
   (let ([w   (+ v (weight-of-point x ys))])
     (cons (list x w) (reweight (dist-remove x ys))))])

;; Rescale a formal sum by a scalar.
(define/match (rescale xss)
  [((list xs v)) (map-on-values (lambda (x) (* v x)) xs)])



;; SUBDISTRIBUTION MONAD.
;; Condensing a formal sum into an unnormalized distribution.
(define (condense xs)
  (reweight
   (remove-zeroes xs)))

;; We implement the combinators for the subdistribution monad; we later use
;; these combinators for the normalized distribution monad.
(define Subd
  (monad
   (lambda (x)    (list (pair x 1)))
   (lambda (xs f) (condense (apply append (map rescale (formal-map f xs)))))
   (lambda (f xs) (formal-map f xs))))

(define dist-return (monad-return Subd))
(define dist-bind   (monad-bind   Subd))
(define dist-map    (monad-map    Subd))

;; Definition of uniform distributions.
(define (dist-uniform ls)
  (define (dist-length ls)
    (apply + (map (lambda (x) 1) ls)))
  (map (lambda (x) (pair x (/ 1 (dist-length ls)))) ls))




;; NORMALIZED DISTRIBUTION MAGMAD.
;; We define normalization.
(define (normalize xs)
  (condense
   (map-on-values (lambda (v) (/ v (validity xs))) xs)))

;; The normalized distribution magmad is similar to the subdistribution monad;
;; the difference is that binding normalizes.
(define Norm
   (monad
    (lambda (x)    (dist-return x))
    (lambda (xs f) (normalize (dist-bind xs f)))
    (lambda (f xs) (dist-map xs f))))

;; The normalized distribution magmad allows observations.
(define (observe x y)
  (if (equal? x y) (uniform '()) (uniform)))

(provide Norm observe)



;; AUXILIARY MACROS FOR DEFINING NORMALIZED DISTRIBUTIONS.
(define-syntax distribution
  (syntax-rules ()
    [(_ [x v] rest ...)  (cons (pair x v) (distribution rest ...))]
    [(_)           (list)]))

(define-syntax uniform
  (syntax-rules ()
    [(_ x ...) (dist-uniform (list x ...))]))

(define (from-table l)
  (map-on-values (lambda (v) (/ v (validity l))) l))

(define-syntax distribution-table
  (syntax-rules ()
    [(_ rest ...)  (from-table (distribution rest ...))]))

(define-syntax define/table
  (syntax-rules ()
    [(_ name (x ...)) (define name (distribution-table x ...))]))

(provide distribution uniform define/table)



;; DISPLAYING DISTRIBUTIONS.
(define (list->string xs)
  (apply format (string-join (make-list (length xs) "~a")) xs))

(define (distribution->string xs)
  (define/match (go xs)
    [('()) ">"]
    [((cons (list x v) xs)) (string-append (format " + ~a[~a]" v (list->string x)) (go xs))])
  (match xs
    ['() "⊥"]
    [(cons (list x v) xs) (string-append (format "<~a[~a]" v (list->string x)) (go xs))]))

;(define (displaydist xs)
;  (displayln (distribution->string xs)))

(struct displaydist (d)
  #:transparent
   #:methods gen:custom-write
  [(define (write-proc x port mode)
     (fprintf port (distribution->string (displaydist-d x))))])

(provide displaydist)

