#lang racket

(require rebellion/collection/multiset)
(require do/dag)
(require do/precondition)
(require do/monad)
(require do/monad/norm)
(require do/leftdo)

(require do/syntax/do)
(require do/derive/conditioning)
(require racket/syntax)


(define (syntax-return ys)
  (with-syntax ([(items ...)  ys])
    (doReturn #'Norm #'(list items ...))))
    


(define (syntax-add-v-suffix stx)
  (format-id stx "~av" stx))

(define exv (list #'a #'c #'b))



;;(doReify #'lDo
;;         (syntax-conditioning (list 'x) (list 'y 'z) (list 'x 'z 'y)))

(define p
  (uniform (list 'blue 'yes 'yes)
           (list 'red 'no 'yes)
           (list 'green 'yes 'no)))


;; SOLUTION.
;; (lambda (b) (lDo Norm
;;      a <- (lDo Norm
;;                (list aa ba ca) <- p
;;                return aa)
;;      c <- (lDo Norm
;;                (list ac bc cc) <- p
;;                '() <- (observe a ac)
;;                '() <- (observe b bc)
;;                return cc)
;;      return (list a c)))


;; PROBLEM
;; (sce (list 'a 'c) (list 'b)
;;      (Dag
;;        'a <- (list)
;;        'b <- (list 'a)
;;        'c <- (list 'a 'b)
;;        visible (list 'a 'b 'c)))

(require racket/list)
(require racket/set)


;; Separated c-component expression.
;; Assuming c-component separation, it computes P(ys|do(xs)).
;; It conditions all variables on all previous non-intervened visible variables.
;; It assumes that P is a distribution on the graph g.
(define (sce ys xs g p)
  (define vs (dag-visibles g))
  (precondition (subset? (list->set xs) (list->set vs))
   'separated-component "intervention variables ~v must be visible variables ~v" xs vs)
  (precondition (subset? (list->set ys) (list->set vs))
   'separated-component "output variables ~v must be visible variables ~v" ys vs)

  (define (acc-sce as ys xs g)
    (match g
      [(dagDependency u _ h)
       (if (or (member u xs) (not (member u vs)))

           ;; #1. Intervened and hidden variables do not appear.
           (if (not (member u vs))
               (acc-sce as ys xs h) ;; hidden
               (acc-sce (cons u as) ys xs h)) ;; intervened

           ;; #2. Visible non-intervening variables are conditioned upon.
           (doStatement #'Norm #`#,u
                        #`#,(doReify #'lDo
                                     (syntax-conditioning (list u) as vs p))
                        (acc-sce (cons u as) ys xs h)))]
      
      [(dagVisible vs)
       (with-syntax ([(yst ...) ys])
         (doReturn #'Norm #'(list yst ...)))]))
  
  ;#`(lambda #,xs #,(doReify #'lDo (acc-sce '() ys xs g))))
  (doReify #'lDo (acc-sce '() ys xs g)))

;; Assuming the ts are given in topological ordering.
;; (define (separated-component-expression ys xs ts p)
;;   (define (sce-acc as ts)
;;     (match ts

;;       [(cons t ts)
;;        ()]
      
;;       ['()
;;        (with-syntax ([(yst ...) ys])
;;          (doReturn #'Norm #'(list yst ...)))]))
;;   )

;; EXAMPLE
;; (sce (list 'a 'c) (list 'b)
;;      (Dag
;;       'a <- (list)
;;       'b <- (list 'a)
;;       'c <- (list 'a 'b)
;;       visible (list 'a 'b 'c)))

(define (equal-sets? as bs)
  (equal?
   (list->set as)
   (list->set bs)))


;; IDENTIFY Algorithm.
;;
;; The inputs are as follows.
;;  - cs output variables
;;  - ts all variables
;;  - q  distribution over ts
;;  - g  semimarkovian graph with visibles in ts.
;;
;; The output is a channel (Ts - Cs) -> Cs, that needs the missing variables, in
;; the order they were received in ts, but filtered.

(define (identify-step cs ts q g)
  ;; #0. The intervention inputs are those not in C.
  (define ins
    (filter (lambda (x) (not (member x cs))) ts))
  
  ;; #1. Compute A = An(C){G{T}}, the ancestors of C in G{T}.
  (define a
    (dag-visible-ancestors cs
      (dag-restricted ts g)))
  
  ;; #2. If A = C, then output the marginal.
  (if (equal-sets? a cs)
      #`(lambda #,ins
          #,(doReify #'lDo (syntax-conditioning cs '() ts q)))

      ;; #3. If A = T, then output failure.
      (if (equal-sets? a ts)
          (error "non identifiable")

          ;; #4. In any other case, we will need a recursive call. 
          ; (identify
          ;   cs
          ;   t-new ; T'
          ;   (sce t-new r-new a)
          ;   )
          (let* ([t-new (dag-c-component-of cs (dag-restricted a g))]
                 [r-new (filter (lambda (x) (not (member x t-new))) a)])
            (sce t-new r-new
                 (dag-restricted ts g) ; g ; (dag-restricted a g)
                 q)))))


(define (identify cs ts q g)
  ;; #0. The intervention inputs are those not in C.
  (define ins
    (filter (lambda (x) (not (member x cs))) ts))
  
  ;; #1. Compute A = An(C){G{T}}, the ancestors of C in G{T}.
  (define a
    (dag-visible-ancestors cs
      (dag-restricted ts g)))
  
  ;; #2. If A = C, then output the marginal.
  (if (equal-sets? a cs)
      #`(lambda #,ins
          #,(doReify #'lDo (syntax-conditioning cs '() ts q)))

      ;; #3. If A = T, then output failure.
      (if (equal-sets? a ts)
          (error "non identifiable")

          ;; #4. In any other case, we will need a recursive call. 
          ; (identify
          ;   cs
          ;   t-new ; T'
          ;   (sce t-new r-new a)
          ;   )
          (let* ([t-new (dag-c-component-of cs (dag-restricted a g))]
                 [r-new (filter (lambda (x) (not (member x t-new))) a)]
                 [q-new (sce t-new r-new (dag-restricted ts g) q)])
            (identify cs t-new q-new (dag-restricted t-new g))))))

;; (?) I assume that ts is always the visibles of q.

;; ;; EXAMPLE
;; (identify '(y) '(x y w) 'q
;;   (Dag
;;      'u1 <- '()
;;      'u2 <- '()
;;      'w <- '(u1 u2)
;;      'x <- '(u1)
;;      'y <- '(u2 x)
;;      visibles '(x y w)))

;; ;; EXAMPLE
;; (identify '(y) '(x y w1 w2 w3 w4 w5) 'p
;;   (Dag
;;    'u1 <- '()
;;    'u2 <- '()
;;    'u3 <- '()
;;    'u4 <- '()
;;    'u5 <- '()
;;    'u6 <- '()
;;    'w1 <- '(u1 u2 u3)
;;    'w2 <- '(w1 u4)
;;    'w3 <- '(u3 u4 u5)
;;    'w4 <- '(w3 u6)
;;    'w5 <- '(u5 u6)
;;    'x <- '(w2 w4 u1)
;;    'y <- '(x u2)
;;    visibles '(x y w1 w2 w3 w4 w5)))


;; (syntax->datum (identify '(y) '(x y w1 w2 w3) 'q
;;   (Dag
;;    'u1 <- '()
;;    'u2 <- '()
;;    'u3 <- '()
;;    'u4 <- '()
;;    'w1 <- '(u1 u2 u3)
;;    'w2 <- '(w1 u4)
;;    'w3 <- '(u3 u4)
;;    'w4 <- '(w3)
;;    'x <- '(w2 w4 u1)
;;    'y <- '(x u2)
;;    visibles '(x y w1 w2 w3 w4))))

;; (identify '(y) '(x y w1) 'q
;;   (Dag
;;    'u1 <- '()
;;    'u2 <- '()
;;    'u3 <- '()
;;    'u4 <- '()
;;    'w1 <- '(u1 u2 u3)
;;    'w3 <- '(u3 u4)
;;    'x <- '(w2 w4 u1)
;;    'y <- '(x u2)
;;    visibles '(x y w1 w2 w3)))


(syntax->datum (identify '(y) '(w x y) 'q
  (Dag
   'u1 <- '()
   'u2 <- '()
   'w <- '(u1 u2)
   'z <- '(w)
   'x <- '(u1)
   'y <- '(x u2)
   visibles '(w x y))))

;; (syntax->datum
;;  (identify '(z3 z2) '(y z3 z2 x) 'q
;;    (Dag
;;      'u1 <- '()
;;      'u2 <- '()
;;      'u3 <- '()
;;      'u4 <- '()
;;      'z2 <- '(u1 u3)
;;      'x  <- '(u1 u2 u4)
;;      'z1 <- '(x)
;;      'z3 <- '(z2 u2)
;;      'y  <- '(z1 z3 u3 u4)
;;      visible '(y z3 z1 x z2))))
