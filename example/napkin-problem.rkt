#lang racket


(require do/monad)
(require do/monad/norm)
(require do/notation/normDo)
(require do/example/data-napkin)
(require do/intervene/dag)
(require do/intervene/algorithm-id)
(require do/intervene/syntax)


;; This file contains a collection of solutions and non-solutions to the napkin
;; problem.


(define (actual-intervention)
  (do (u1) <- sigma1
      (u2) <- sigma2
      (w) <- (f u1 u2)
      (z) <- (g w)
      (x) <- (do (x) <- (h u1 z)
                 () <- (observe x 'p)
                 return (x))
      (y) <- (k u2 x)
      return (y)))

(require do/example/data-napkin)
(require do/monad/norm)
(require do/intervene/simplify-unitality)
(require do/intervene/derive-interventions)

(define napkin
  (Dag
   'u1 <- '()
   'u2 <- '() 
   'w <- '(u1 u2)
   'z <- '(w)
   'x <- '(z u1)
   'y <- '(x u2)
   visible '(w z x y)))

(InterveneStx p
  WithModel (do
                u1 <- ()
                u2 <- ()
                w <- (u1 u2)
                z <- (w)
                x <- (z u1)
                y <- (x u2)
                visible (w z x y))
  Setting (x) To ('p) In (y))

(define (example-napkin)
   (algorithm-id (normProgram #'p) napkin '(x) '(y)))

(define (solution)
  (define z 'u)
  (define x 'p)
  (do 
    (x52 y53) <- (do 
            (x49 w50 y51) <- (do 
                    (w) <- (do 
                            (w37 z38 x39 y40) <- p
                            return (w37))
                    (x) <- (do 
                            (w41 z42 x43 y44) <- p
                            () <- (observe z z42)
                            () <- (observe w w41)
                            return (x43))
                    (y) <- (do 
                            (w45 z46 x47 y48) <- p
                            () <- (observe x x47)
                            () <- (observe z z46)
                            () <- (observe w w45)
                            return (y48))
                    return (x w y))
            return (x49 y51))
    () <- (observe x x52)
    return (y53)))



;; (define (jacobs-solution)
;;   (lDo Norm
;;      w <- (lDo Norm
;;                (list w z x y) <- p
;;                return w)
;;      z <- (lDo Norm
;;                (list w0 z x y) <- p
;;                return z)
;;      (list x y) <- (lDo Norm
;;                (list w0 z0 x y) <- p
;;                '() <- (observe w w0)
;;                '() <- (observe z z0)
;;                return (list x y))
;;      '() <- (observe x 'p)
;;      return y))


;; (define (jacobs-non-solution)
;;   (lDo Norm
;;      w <- (lDo Norm
;;                (list w z x y) <- p
;;                return w)
;;      z <- (lDo Norm
;;                (list w z x y) <- p
;;                return z)
;;      y <- (lDo Norm
;;                (list w0 z0 x y) <- p
;;                '() <- (observe w w0)
;;                '() <- (observe z z0)
;;                '() <- (observe x 'p)
;;                return y)
;;      return y))


;; (define (alternative-solution)
;; (lDo Norm
;;      w <- (lDo Norm
;;                (list w z x y) <- p
;;                return w)
;;      z <- (lDo Norm
;;                (list w0 z x y) <- p
;;                '() <- (observe w0 w)
;;                return z)
;;      (list x y) <- (lDo Norm
;;                (list w0 z0 x y) <- p
;;                '() <- (observe w w0)
;;                return (list x y))
;;      '() <- (observe x 'p) 
;;      return y))

;; (define (modified-solution)
;;   (lDo Norm
;;      (list x y) <- (lDo Norm
;;                         w <- (lDo Norm
;;                                   (list w z x y) <- p
;;                                   return w)                   
;;                         z <- (lDo Norm
;;                                   (list w z x y) <- p
;;                                   return z)
;;                         (list x y) <- (lDo Norm
;;                                            (list w0 z0 x y) <- p
;;                                            '() <- (observe w w0)
;;                                            '() <- (observe z z0)
;;                                            return (list x y))
;;                         return (list x y))
;;      '() <- (observe x 'p)
;;      return y))

;; (define (my-solution)
;; (lDo Norm
;;      z <- (lDo Norm
;;                (list w0 z x y) <- p
;;                return z)

;;      w <- (lDo Norm
;;                w <- (lDo Norm
;;                          (list w z x y) <- p
;;                          return w)                   
;;                x1 <- (lDo Norm
;;                           (list w0 z0 x y) <- p
;;                           '() <- (observe w w0)
;;                           '() <- (observe z z0)
;;                           return x)
;;                '() <- (observe x1 'p)
;;                return w)
     
;;      (list y w2) <- (lDo Norm
;;                (list w1 x1 y) <- (lDo Norm
;;                                    w <- (lDo Norm
;;                                              (list w z x y) <- p
;;                                              return w)                   
;;                                    x1 <- (lDo Norm
;;                                               (list w0 z0 x y) <- p
;;                                               '() <- (observe w w0)
;;                                               '() <- (observe z z0)
;;                                               return x)
;;                                    y <- (lDo Norm
;;                                              (list w0 z0 x0 y) <- p
;;                                              '() <- (observe w w0)
;;                                              '() <- (observe z z0)
;;                                              '() <- (observe 'p x0)
;;                                              return y)
;;                                    return (list w x1 y))
;;                '() <- (observe x1 'p)
;;                '() <- (observe w1 w)
;;                return (list y w1))
;;      return y))
;; (my-solution)

;; (define (my-other-solution)
;; (lDo Norm
;;      ;; ID(Y <- X; WZXY)
;;      ;; IDENTIFY(W; WXY)
;;      w <- (lDo Norm
;;                (list w0 z0 x0 y0) <- p
;;                return w0)
;;      ;; IDENTIFY(Z; Z), input on w
;;      z <- (lDo Norm
;;                (list w1 z1 x1 y1) <- p
;;                '() <- (observe w1 w)
;;                return z1)
;;      ;; IDENTIFY(Y; WXY), input on z; intervention on x
;;      y <- (lDo Norm
;;                ;; IDENTIFY(XY; WXY), input on z; (!) non-input w
;;                wa <- (lDo Norm
;;                          (list w z x y) <- p
;;                          return w)
;;                x1 <- (lDo Norm
;;                           w <- (lDo Norm
;;                                     (list w z x y) <- p
;;                                     return w)
;;                           x1 <- (lDo Norm
;;                                      (list w0 z0 x y) <- p
;;                                      '() <- (observe w w0)
;;                                      '() <- (observe z z0)
;;                                      return x)
;;                           '() <- (observe w wa)
;;                           return x1)
;;                y <- (lDo Norm
;;                          w <- (lDo Norm
;;                                    (list w z x y) <- p
;;                                    return w)
;;                          x2 <- (lDo Norm
;;                                     (list w0 z0 x y) <- p
;;                                     '() <- (observe w w0)
;;                                     '() <- (observe z z0)
;;                                     return x)
;;                          y <- (lDo Norm
;;                                    (list w0 z0 x0 y) <- p
;;                                    '() <- (observe w w0)
;;                                    '() <- (observe z z0)
;;                                    '() <- (observe 'p x0)
;;                                    return y)
;;                          '() <- (observe w wa)
;;                          '() <- (observe x1 x2)
;;                          return y)
;;                '() <- (observe x1 'p)
;;                return y)
;;      return y))


;; (define (automatic-non-solution)
;; (lDo Norm
;;      ;; ID(Y <- X; WZXY)

;;      ;; IDENTIFY(W; WXY)
;;      w <- (lDo Norm
;;                (list w0 z0 x0 y0) <- p
;;                return w0)

;;      ;; IDENTIFY(Z; Z), input on w
;;      z <- (lDo Norm
;;                 (list w1 z1 x1 y1) <- p
;;                 '() <- (observe w1 w)
;;                 return z1)

;;      ;; IDENTIFY(Y; WXY), input on z; intervention on x
;;      y <- (lDo Norm
;;                w <- (lDo Norm
;;                           (list w1994 z x1995 y1996) <- p
;;                           return w1994)
;;                y <- (lDo Norm
;;                          (list w1997 z x1998 y1999) <- p
;;                          '() <- (observe 'p x1998)
;;                          '() <- (observe w w1997)
;;                          return y1999)
;;                return y)
;;      return y))



;; (define (non-solution)
;; (lDo Norm
;;      (list x y) <- (lDo Norm
;;                         (list w z x y) <- p
;;                         return (list x y))
;;      '() <- (observe x 'p)
;;      return y))


(provide p)
