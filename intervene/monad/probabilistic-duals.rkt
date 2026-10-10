#lang racket

(require do/monad/duals)

;; PROBABILISTIC DUALS.
;;
;; INTRODUCTION.
;;
;; Dual interval implemented in probabilistic form. The differential part is not
;; dependent on the real part.

(struct b (real piff)
  #:transparent
  #:methods gen:custom-write
  ((define (write-proc x port mode)
    (fprintf port
             (if (equal? (b-piff x) 0)
                 (format "~a" (b-real x))
                 (if (< (b-piff x) 0)
                     (format "~a~adB" (b-real x) (~r (b-piff x) #:precision '(= 2) ))
                     (format "~a+~adB" (b-real x) (~r (b-piff x) #:precision '(= 2) ))))))))


;; DEFINITION. Logistic and Logit functions
;;
;; Two mutual inverses translating from the interval to the reals. Probability
;; can be expressed in terms of decibels.

;; Logistic : Real[inf] -> Interval.
;; 
;; Handles +infinity and -infinity.
(define (logistic x)
  (define (logistic-function x)
    (/ (exp x) (+ 1 (exp x))))
  (if (= x +inf.0) 1 
      (if (= x -inf.0) 0
          (logistic-function x))))
  

;; Logit : Interval -> Real
(define (logit p)
  (define (logit-function p)
    (log (/ p (- 1 p))))

  (if (= p 1) +inf.0
      (if (= p 0) -inf.0
          (logit-function p))))


;; REMARK. Numerical instability
;;
;; These functions start breaking at around 100dB. There, the reals already
;; collapse probability to 1, and the functions do not work as inverses.


;; DEFINE. Translations.

(define/match (b->d x)
  [((b xr xb))
    (d xr (- (logistic (+ (logit xr) xb)) xr))])

(define/match (d->b x)
  [((d xr xd))
   (b xr (- (logit (+ xr xd)) (logit xr)))])


;; PROBABILISTIC DUALS

(struct p (real diff)
  #:transparent
  #:methods gen:custom-write
  ((define (write-proc x port mode)
     (fprintf port
              (format "~a" (d->b (d (p-real x) (p-diff x))))
              ))))

(define (db u v)
  (match (b->d (b u v))
    [(d x y) (p x y)]))
