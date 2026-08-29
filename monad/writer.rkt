#lang racket

(require do/monad)
(require do/monoid)

(define (Writer M)
  (monad
    ;; return : x -> m * x
    (λ (x) (cons (monoid-unit M) x))

    ;; bind : m * x -> (x -> m * y) -> m * y
    (λ (xs f) (cons ((monoid-multiplication M) (car xs) (car (f (cdr xs))))
                    (cdr (f (cdr xs)))))

    ;; map : (x -> y) -> (m * x -> m * y)
    (λ (f xs) (cons (car xs) (f (cdr xs))))))

(provide Writer)
(provide monoid)
