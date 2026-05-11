#lang racket

(require rackunit)
(struct dagDependency (out ins more) #:transparent)
(struct dagVisible (outs) #:transparent)

(define (check-equal-sets? a b)
  (check-equal? (list->set a) (list->set b)))

(define (dagParse stx)
  (syntax-case stx (visible)
    [(_ o <- is more ...)
     (dagDependency (syntax->datum #'o) (syntax->datum #'is) (dagParse #'(dummyDag more ...)))]
    [(_ visible xs) (dagVisible (syntax->datum #'xs))]))

(define-syntax Dag
  (syntax-rules (<-)
    [(Dag o <- is more ...)  (dagDependency o is (Dag more ...))]
    [(Dag visible xs)        (dagVisible xs)]))

(define (dag-visibles g)
  (match g
    [(dagDependency _ _ more) (dag-visibles more)]
    [(dagVisible vs) vs]))

(define (dag-hidden g)
  (define (dag-hidden-acc acc g)
    (match g
      [(dagDependency a bs h) (dag-hidden-acc (cons a acc) h)]
      [(dagVisible vs) (filter (lambda (x) (not (member x vs))) acc)]))
  (dag-hidden-acc '() g))


(check-equal-sets?
 ;; <== example
 (dag-hidden
   (Dag
   'z <- (list)
   'w <- (list 'z)
   'y <- (list)
   'x <- (list 'y 'z)
   visible (list 'x 'y)))
 ;; ==> answer
 (list 'z 'w))



;; Removes completely all of the occurrences of a list of variables on the
;; graph.
(define (dag-remove ws g)
  (define (list-remove-vars l)
    (filter (lambda (x) (not (member x ws))) l))
  
  (match g

    [(dagDependency a bs more)
     (if (member a ws)
         (dag-remove ws more)
         (dagDependency a (list-remove-vars bs) (dag-remove ws more)))]

    [(dagVisible vs)
     (dagVisible (list-remove-vars vs))]))

;; EXAMPLE
;; (dag-remove (list 'y)
;;   (Dag
;;    'z <- (list)
;;    'y <- (list)
;;    'x <- (list 'y 'z)
;;    visible (list 'x 'y)))

(define (dag-restricted cs g)
  (match g

    [(dagDependency a bs h)
     (if (or (member a cs) (member a (dag-hidden g)))
         (dagDependency a bs (dag-restricted cs h))
         (dag-restricted cs h))]
    [(dagVisible vs) (dagVisible (filter (lambda (x) (member x cs)) vs))]))

(define (dag-ancestors cs g)
  (match g
    [(dagDependency a bs h)
     (let ([acs (dag-ancestors cs h)])
       (if (member a acs)
           (append bs acs) acs))]
    [(dagVisible vs) cs]))

(define (dag-visible-ancestors cs g)
  (filter (lambda (x) (member x (dag-visibles g)))
            (dag-ancestors cs g)))

(define (dag-topological-sort-of vs g)
  (match g
    [(dagDependency a _ h)
     (if (member a vs)
         (cons a (dag-topological-sort-of vs h))
         (dag-topological-sort-of vs h))]
    [(dagVisible ws) '()]))

;; EXAMPLE
;; (dag-visible-ancestors (list 'x)
;;    (Dag
;;    'z <- (list)
;;    'w <- (list 'z)
;;    'y <- (list)
;;    'x <- (list 'y 'z)
;;    visible (list 'x 'y)))


(define (dag-parents a g)
  (match g
    [(dagDependency x ps h)
     (if (equal? a x) ps (dag-parents a h))]
    [(dagVisible v)         '()]))

;; EXAMPLE
;; (dag-parents 'x
;;    (Dag
;;    'z <- (list)
;;    'w <- (list 'z)
;;    'y <- (list)
;;    'x <- (list 'y 'z 'w)
;;    visible (list 'x 'y)))

(define (member? x l)
  (if (member x l) #t #f))

(define (dag-confounders? g a b)
   (ormap (lambda (x) (and (member? x (dag-parents a g))
                           (member? x (dag-parents b g))))
          (dag-hidden g)))

;; example
(check-true (dag-confounders? 
 (Dag
  'z <- (list)
  'w <- (list 'z)
  'y <- (list 'z 'w)
  'x <- (list 'z)
  visible (list 'x 'y 'w))
 'x 'y))

(require do/graph-components)

(define (dag-c-components g)
  (partition-by-relation
   (dag-visibles g)
   (lambda (x y) (dag-confounders? g x y))))


(check-equal?
 ; => example
 (dag-c-components
  (Dag
    'u1 <- '()
    'u2 <- '()
    'w1 <- '(u1 u2)
    'w2 <- '(w1)
    'x <- '(u1 w2)
    'y <- '(x u2)
    visible '(x y w1 w2)))
 ; => answer
 '((w2) (x w1 y)))

;; DAG-C-COMPONENT-OF.
;; In a semimarkovian `graph`, pick the c-component of a variable `v`.
(define (dag-c-component-of v graph)
  (define (find-in-partition x p)
    (match p
      [(cons u p)  (if (member x u) u (find-in-partition x p))]
      ['()         (error "dag-c-component-of not here" graph v (dag-c-components graph))]))

  (dag-topological-sort-of (find-in-partition v (dag-c-components graph)) graph))

(check-equal?
 ;; ==> Example
 (dag-c-component-of 'x
   (Dag
    'u1 <- '()
    'u2 <- '()
    'w <- '(u1 u2)
    'x <- '(w u1)
    'z <- '(x)
    'y <- '(z u2)
    visible '(w x z y)))
 ;; <== Answer
 '(w x y))


;; DAG-C-COMPONENT-UNTIL.

;; In a semimarkovian `graph`, pick the c-component of a variable `v`, but only
;; list variables before v. The variables are topologically ordered.
(define (dag-c-component-until v graph)
  (define (list-until v l)
    (match l
      [(cons u l)  (if (equal? u v) (list v) (cons u (list-until v l)))]
      ['()         (error "not here")]))

  (list-until v (dag-c-component-of v graph)))

(check-equal?
 ;; ==> Example
 (dag-c-component-until 'x
   (Dag
    'u1 <- '()
    'u2 <- '()
    'w <- '(u1 u2)
    'x <- '(w u1)
    'z <- '(x)
    'y <- '(z u2)
    visible '(w x z y)))
 ;; <== Answer
 '(w x))



(define (dag-c-component-of-vars cs g)
  (define (find-in-partition x p)
    (match p
      [(cons u p)
       (if (member x u) u
           (find-in-partition x p))]
      ['()  (error "not here")]))

  (find-in-partition (first cs) (dag-c-components g)))

(check-equal-sets? 
 ; => example
 (dag-c-component-of-vars '(x y)
  (Dag
    'z <- '()
    'w <- '(z)
    'y <- '(z w)
    'x <- '(z)
    visible '(x y w))) 
 ; => answer 
  '(x y w))


(provide (struct-out dagDependency))
(provide (struct-out dagVisible))
(provide Dag)
(provide dag-visibles)
(provide dag-hidden)
(provide dag-remove)
(provide dag-restricted)
(provide dag-ancestors)
(provide dag-visible-ancestors)
(provide dag-c-components)
(provide dag-c-component-of)
(provide dag-c-component-until)
(provide dag-topological-sort-of)
(provide dagParse)
