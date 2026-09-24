#lang racket

(require do/monad)

;; Dual numbers.
(struct d (real diff)
  #:transparent
  #:methods gen:custom-write
  ((define (write-proc x port mode)
    (fprintf port
             (if (< (d-diff x) 0)
                 (format "~a~aε" (d-real x) (d-diff x))
                 (format "~a+~aε" (d-real x) (d-diff x)))))))


;; Addition.
(define/match (d+bin x y)
  [((d xr xd) (d yr yd))
   (d
     (+ xr yr)
     (+ xd yd))])

(define (d+null) (d 0 0))

(define (d+ . args)
  (foldr (lambda (x y) (d+bin x y)) (d+null) args))


;; Multiplication.
(define/match (d*bin x y)
  [((d xr xd) (d yr yd))
   (d
     (* xr yr)
     (+ (* xr yd) (* xd yr)))])

(define (d*null) (d 1 0))

(define (d* . args)
  (foldr (lambda (x y) (d*bin x y)) (d*null) args))


;; Division.
(define/match (d/bin x y)
  [((d x a) (d y b))
   (d
     (/ x y)
     (/ (- (* y a) (* x b)) (* y y)))])


