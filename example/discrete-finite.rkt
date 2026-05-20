#lang racket

(require rackunit)
(require do/monad)
(require do/monad/norm)
(require do/leftdo)

(define (check-equal-sets? a b)
  (check-equal? (list->set a) (list->set b)))


;; X Z -> Y
(define (p x z)
  (match (list x z)
    [(list 'blue 'blue) (distribution ['red 1/2] ['green 1/2])]
    [(list 'blue 'red) (distribution ['blue 1/2] ['green 1/2])]
    [(list 'blue 'green) (distribution)]
    [(list 'red 'blue) (distribution ['red 3/4] ['blue 1/4])]
    [(list 'red 'red) (distribution)]
    [(list 'red 'green) (distribution)]
    [(list 'green 'blue) (distribution ['red 3/4] ['blue 1/4])]
    [(list 'green 'red) (distribution ['blue 1/2] ['green 1/2])]
    [(list 'green 'green) (distribution ['blue 2/5] ['red 2/5] ['green 1/5])]
    ))

;; One side is compact closed.
(define (udp x z)
  (lDo Norm
       x0 <- (uniform 'blue 'red 'green)
       y <- (p x0 z)
       '() <- (observe x0 x)
       return y))

(for* ([i (list 'blue 'green 'red)]
      [j (list 'blue 'green 'red)])
  (check-equal? (udp i j) (p i j)))


;; The other one.
(define (dup x z)
  (lDo Norm
       x0 <- (uniform 'blue 'red 'green)
       y <- (lDo Norm
                 y <- (p x0 z)
                 '() <- (observe x0 x)
                 return y)
       return y))

(for* ([i (list 'blue 'green 'red)]
       [j (list 'blue 'green 'red)])
   (check-equal? (dup i j) (p i j)))


;; Z -> X Y
(define (q z)
  (match z
    ['blue (distribution [(list 'red 'red) 2/4] [(list 'red 'blue) 1/4] [(list 'blue 'blue) 1/4])]
    ['red (distribution [(list 'green 'red) 1/3] [(list 'red 'blue) 2/3])]
    ['green (distribution)]))


(define (udq z)
  (lDo Norm
       x <- (uniform 'red 'green 'blue)
       (list x0 y) <- (q z)
       '() <- (observe x0 x)
       return (list x y)))

(for* ([i (list 'blue 'green 'red)])
   (check-equal-sets? (udq i) (q i)))


(define (udq2 z)
  (lDo Norm
       x <- (uniform 'red 'green 'blue)
       y <- (lDo Norm
                 (list x0 y) <- (q z)
                 '() <- (observe x0 x)
                 return y)
       return (list x y)))

(for* ([i (list 'blue 'green 'red)])
  (displayln (list i))
  (check-equal-sets? (udq2 i) (q i)))
