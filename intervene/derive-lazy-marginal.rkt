#lang racket

(require do/intervene/syntax)
(require do/intervene/syntax-helpers)

(define (get-useful-variables P S)
  (define (go P)
    (match P
      [(normReturn X) S]
      [(normObservation x y Q) (append (list x y) (go Q))]
      [(normStatement X F Q) (get-useful-variables F (go Q))]
      [q S]))
  (match P
    [(normProgram P) (go P)]))

(define (lazy-marginal P V S)
  (define (go P)
    (match P
      [(normReturn X) (normReturn S)]
      [(normObservation x y Q) (normObservation x y (go Q))]
      [(normStatement X F Q) (let ([U (intersect-list X (get-useful-variables (normProgram Q) S))])
                               (normStatement U (lazy-marginal F X U) (go Q)))]
      [q (normStatement V (normProgram q) (normReturn S))]))
  (match P [(normProgram P) (normProgram (go P))]))

(define (example-variables)
  (get-useful-variables
   (normProgram
    (normStatement '(x y) (normProgram #'p)
       (normObservation 'u 'v
          (normReturn '(x))))) '(x)))

(define (example-data-marginal)
  (normProgram
    (normStatement '(x y) (normProgram #'p)
      (normStatement '(z) (normProgram (normObservation 'x 'x (normReturn '(z))))
                     (normObservation 'u 'v 
                                      (normReturn '(x y z)))))))
(define (example-marginal)
  (lazy-marginal (example-data-marginal)
    '(x y z) '(x)))
