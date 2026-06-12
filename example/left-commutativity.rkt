#lang racket

(require rackunit)
(require do/monad/norm)
(require do/notation/unbias/leftDo)

;; This example illustrates left-commutativity.

;; Partial stochastic
;; f : Dir -> Color x Color
(define (f x)
  (match x
    ['left  (distribution ['(green red) 2/4] ['(blue green) 1/4] ['(blue red) 1/4])]
    ['right (uniform '())]))

;; Partial stochastic
;; g : Dir -> Dir
(define (g x)
  (match x
    ['left (uniform)]
    ['right (distribution ['(left) 1/5] ['(right) 4/5])]))

;; Stochastic
;; h : Color x Dir -> Color
(define (h y x)
  (match x
    ['left  (match y
              ['blue  (distribution ['(green) 2/4] ['(blue) 1/4] ['(red) 1/4])]
              ['green (distribution ['(red) 2/3] ['(green) 1/3])]
              ['red   (distribution ['(green) 1/7] ['(blue) 1/7])])]
    ['right (distribution ['(green) 1/3] ['(red) 2/3])]))


;; Left-hand side.
(define (eq-lhs a b c)
  (do Norm
       (x y) <- (f a)
       (u) <- (g b)
       (v) <- (h y c)
       return (x u v)))

;; Right-hand side.
(define (eq-rhs a b c)
  (do Norm
       (x y) <- (f a)
       (v) <- (h y c)
       (u) <- (g b)
       return (x u v)))


