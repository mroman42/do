#lang racket

(require do/monad/norm)

(struct Ret
  (index value))

(struct Expr
  (input output generator))


(define semaphor
  (Expr 0 (list 1 1 1)
        (lambda (k1 k2 k3)
          (norm-join (distribution
                      [(k1 '(g)) 1/3]
                      [(k2 '(r)) 1/3]
                      [(k3 '(b)) 1/3])))))

(define coin
  (Expr 0 (list 0 0)
        (lambda (k1 k2)
          (norm-join (distribution
           [(k1 '()) 1/2]
           [(k2 '()) 1/2])))))

(define example
  (Expr 0 (list 0 0 0 0)
  (lambda (k1 k2 k3 k4)
     ((Expr-generator semaphor)
      (lambda (green)
        ((Expr-generator coin)
         (lambda (heads) (k1 'a))
         (lambda (tails) (k2 'b))))
      (lambda (blue) (k3 'a))
      (lambda (red) (k4 'a))))))

