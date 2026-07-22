#lang racket

(require racket/match)
(require do/intervene/dag)
(require do/intervene/syntax)
(require do/intervene/syntax-helpers)


(define (change-return us program)
  (define (go p)
    (match p
      [(normReturn vars) (normReturn us)]
      [(normObservation a b q) (normObservation a b (go q))]
      [(normStatement vs m q) (normStatement vs m (go q))]
      [q (normStatement us (normProgram q) (normReturn us))]))
  (match program
    [(normProgram p) (normProgram (go p))]))
  


(define (usefulVars program)
  (define (go p)
    (match p
      ; All returned variables are useful.
      [(normReturn vars) vars]
      
      ; All observed variables are useful.
      [(normObservation x y q) (append (go q) (list x y))]

      ; In a statement, all variables needed for the useful variables are useful.
      [(normStatement  vs m q) (let* ([uVars (go q)]
                                      [nVs   (filter (lambda (x) (member x uVars)) vs)]
                                      [mVars (usefulVars m)])
                                 (append uVars mVars))]
      [q '()]))
  (match program
    [(normProgram p) (go p)]))


(define (simplify-totality program)
  (define (go p)
    (match p
      [(normReturn vars) (normReturn vars)]
      [(normObservation x y q) (normObservation x y (go p))]
      [(normStatement vs m q) (let* ([uVars (usefulVars (normProgram q))]
                                     [nVs   (filter (lambda (x) (member x uVars)) vs)]
                                     [mSimp (simplify-totality (change-return nVs m))]
                                     [mVars (usefulVars mSimp)])
                                (normStatement nVs mSimp (go q)))]
      [q q]))
  (match program
    [(normProgram p) (normProgram (go p))]))

(define (example-simplify-totality)
  (usefulVars
   (normProgram
   (normStatement '(x) (normProgram #'p)
    (normStatement '(y) (normProgram #'p)
      (normStatement '(z) (normProgram
                           (normObservation 'x 'x
                            (normStatement '(z) (normProgram #'q)
                             (normReturn '(z)))))
        (normReturn '(z))))))))







