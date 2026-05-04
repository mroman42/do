#lang racket

{begin-for-syntax

(require racket/list)

(struct dagDependency (out ins more) #:transparent)
(struct dagVisible (outs) #:transparent)

(define (dagParse stx)
  (syntax-case stx ()
    [(_ visible vs)
     (dagVisible (rest (syntax->list #'vs)))]
    [(_ o <- is more ...)
     (dagDependency #'o (rest (syntax->list #'is))
                    (dagParse #'(dummyDagParse more ...)))]))

}

(define-syntax (dagParse stx)
  (with-syntax ([stx-transformed (dagParse stx)])
    #'stx-transformed))


;; (dagParse
;;    'z <- (list)
;;    'y <- (list)
;;    'x <- (list 'y 'z)
;;    visible (list 'x 'y))


(struct dagDependency (out ins more) #:transparent)
(struct dagVisible (outs) #:transparent)

(define-syntax Dag
  (syntax-rules (<-)
    [(Dag o <- is more ...)  (dagDependency o is (Dag more ...))]
    [(Dag visible xs)        (dagVisible xs)]))

;; (Dag
;;    'z <- (list)
;;    'y <- (list)
;;    'x <- (list 'y 'z)
;;    visible (list 'x 'y))



(require leftdo/monad)
(require leftdo/monad/norm)
(require leftdo/leftdo)

(require leftdo/syntax/do)


(define (syntax-return ys)
  (with-syntax ([(items ...)  ys])
    (doReturn #'Norm #'(list items ...))))
    

(define (sce os i g)
  (match g
    [(dagVisible ys) (syntax-return ys)]
    [(dagDependency out ins more)
     (with-syntax ([out-s out] [(ins-s ...) ins])
       (doStatement #'Norm #'out-s #'(list ins-s ...) (sce os i more)))]
    ))

;; (doReify #'lDo
;;   (sce (list) (list)
;;      (Dag
;;       'a <- (list)
;;       'b <- (list 'a)
;;       visible (list 'a 'b 'c))))


(define (syntax-conditioning xs ys vs)
  (with-syntax
    ([(vss ...) vs])
    (doStatement #'Norm #'(vss ...) #'p
               (doStatement #'Norm #'(list) #'(observe #,ys ys)
                            (doReturn #'Norm #'xs)))))

(doReify #'lDo
         (syntax-conditioning (list 'x) (list 'y) (list 'x 'z 'y)))


(define p
  (uniform (list 'blue 'yes 'yes)
           (list 'red 'no 'yes)
           (list 'green 'yes 'no)))

;; (sce (list 'a 'b) (list 'c)
;;      (Dag
;;        'a <- (list)
;;        'c <- (list 'a)
;;        'b <- (list 'a 'c)
;;        visible (list 'a 'b 'c)))

;; SOLUTION.
(lDo Norm
     a <- (lDo Norm
               (list aa ba ca) <- p
               return aa)
     c <- (lDo Norm
               (list ac bc cc) <- p
               '() <- (observe a ac)
               '() <- (observe bc 'yes)
               return cc)
     return (list a c))

(lDo Norm
     a <- (uniform 3)
     return (list a))
