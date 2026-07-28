#lang racket

(define (equal-sets? as bs)
  (equal?
   (list->set as)
   (list->set bs)))

(define (syntaxSymbol->string x)
  ;(symbol->string (syntax->datum x)))
  (format "~a" (syntax->datum x)))

(define (symbols->string xs)
  (match xs
    [(cons x xs) (string-append (symbol->string x) " " (symbols->string xs))]
    ['() ""]))

(define (syntaxVars->string vs)
  (define (go vs)
    (match vs
      [(cons v vs) (string-append " " (syntaxSymbol->string v) (go vs))]
      ['() ""]))
  (string-append "(" (go vs) ")"))

(define (equalDatum? x y)
  (equal? (syntax->datum x) (syntax->datum y)))

(define (syntax->data xs)
  (map (lambda (x) (syntax->datum x)) xs))

(define (symbols->listString xs)
  (define (go xs)
    (match xs
      [(cons x xs) (string-append " " (symbol->string x) (go xs))]
      ['() ""]))
  (match xs
    [(cons x xs) (string-append "(" (symbol->string x) (go xs) ")")]
    ['() "()"]))

(define (memberDatum? x xs)
  (member (syntax->datum x) (syntax->data xs)))

(define (until l x)
      (define (go acc x l)
        (match l
          ['() acc]
          [(cons y lr) (if (equal? x y) (append acc (list x)) (go (append acc (list y)) x lr))]))
      (go '() x l))

(define (list-before x l)
  (define (go acc x l)
        (match l
          ['() acc]
          [(cons y lr) (if (equal? x y) acc (go (append acc (list y)) x lr))]))
      (go '() x l))

(define (intersect-list p q)
  (filter (lambda (x) (member x q)) p))

(provide syntaxSymbol->string)
(provide syntaxVars->string)
(provide syntax->data)
(provide equalDatum?)
(provide memberDatum?)
(provide symbols->string)
(provide symbols->listString)
(provide equal-sets?)
(provide until)
(provide intersect-list)
(provide list-before)
