#lang racket

(require "magmad.rkt")
(require "rightDo.rkt")

;; This is an implementation of Definition 4.13.
(define-syntax leftDo
  (syntax-rules (<- return)

    ;; This is clause (i) in Definition 4.13.
    [
     (leftDo T
             (x ...) <- m1
             (y ...) <- m2
             rest ...)
     (leftDo T
             (x ... y ...) <- (rightDo T
                                (x ...) <- m1
                                (y ...) <- m2
                                return (x ... y ...))
             rest ...)
     ]

    ;; This is clause (ii) in Definition 4.13.
    [(leftDo T
             (x ...) <- f
             return (y ...))
     (rightDo T
          (x ...) <- f
          return (y ...))
    ]

    ;; This is clause (iii) in Definition 4.13.
    [(leftDo T return (y ...))
     (rightDo T return (y ...))]
    
    ))

(define-syntax do
  (syntax-rules (<- return)
    [(do i ...) (leftDo i ...)]))


(provide leftDo do)
(provide (all-from-out "magmad.rkt"))
(provide (all-from-out "rightDo.rkt"))
