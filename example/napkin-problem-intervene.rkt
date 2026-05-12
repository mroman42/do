#lang racket

(require do/intervene/identify)
(require do/monad)
(require do/monad/norm)

(define pure (monad-return Norm))

(define p
  (distribution
   ((list 'x 'u 'p 'p) 389/90720)
   ((list 'x 'u 'p 'q) 2167/90720)
   ((list 'x 'u 'q 'p) 94/1575)
   ((list 'x 'u 'q 'q) 689/6300)
   ((list 'x 'v 'p 'p) 389/38880)
   ((list 'x 'v 'p 'q) 2167/38880)
   ((list 'x 'v 'q 'p) 94/2025)
   ((list 'x 'v 'q 'q) 689/8100)
   ((list 'y 'u 'p 'p) 37/58320)
   ((list 'y 'u 'p 'q) 1577/408240)
   ((list 'y 'u 'q 'p) 19/2025)
   ((list 'y 'u 'q 'q) 499/28350)
   ((list 'y 'v 'p 'p) 259/21870)
   ((list 'y 'v 'p 'q) 1577/21870)
   ((list 'y 'v 'q 'p) 1064/18225)
   ((list 'y 'v 'q 'q) 1996/18225)
   ((list 'z 'u 'p 'p) 31/7560)
   ((list 'z 'u 'p 'q) 143/7560)
   ((list 'z 'u 'q 'p) 53/1050)
   ((list 'z 'u 'q 'q) 46/525)
   ((list 'z 'v 'p 'p) 31/3240)
   ((list 'z 'v 'p 'q) 143/3240)
   ((list 'z 'v 'q 'p) 53/1350)
   ((list 'z 'v 'q 'q) 46/675)))

;; ((Identify2 (y) Forcing (x)
;;             (do u1 <- ()
;;                 u2 <- ()
;;                 w <- (u1 u2)
;;                 z <- (w)
;;                 x <- (z u1)
;;                 y <- (x u2)
;;                 return (w z x y))
;;             (lDo Norm
;;                  (list a b c d) <- p
;;                  return (list a b c d))) 'p)

;; (Identify (y)
;;  Intervening (x)
;;  To ('p)
;;  WithModel (do u1 <- ()
;;         u2 <- ()
;;         w <- (u1 u2)
;;         z <- (w)
;;         x <- (z u1)
;;         y <- (x u2)
;;         return (w z x y))
;;  (lDo Norm
;;       (list a b c d) <- p
;;       return (list a b c d)))


;(napkin-solution 'p)

;; EXAMPLE 2. Smoking causes cancer.
(define survey
  (distribution
     [(list 'smoker 'tar 'nocancer)    323/800]
     [(list 'smoker 'tar 'cancer)       57/800]
     [(list 'nonsmoker 'tar 'nocancer)    1/800]
     [(list 'nonsmoker 'tar 'cancer)     19/800]
     [(list 'smoker 'notar 'nocancer)   18/800]
     [(list 'smoker 'notar 'cancer)      2/800]
     [(list 'nonsmoker 'notar 'nocancer) 38/800]
     [(list 'nonsmoker 'notar 'cancer)  342/800]))

(lDo Norm
     habits <- (distribution ['smoker 2/10] ['nonsmoker 8/10])
     (list smoking tar cancer) <-
       (Intervene (distribution
                   [(list 'smoker 'tar 'nocancer)    323/800]
                   [(list 'smoker 'tar 'cancer)       57/800]
                   [(list 'nonsmoker 'tar 'nocancer)    1/800]
                   [(list 'nonsmoker 'tar 'cancer)     19/800]
                   [(list 'smoker 'notar 'nocancer)   18/800]
                   [(list 'smoker 'notar 'cancer)      2/800]
                   [(list 'nonsmoker 'notar 'nocancer) 38/800]
                   [(list 'nonsmoker 'notar 'cancer)  342/800])
                  WithModel (do genes <- ()
                                smoking <- (genes)
                                tar <- (smoking)
                                cancer <- (tar genes)
                                return (smoking tar cancer))
                  Setting (smoking) To (habits))
       return cancer)

;; (lDo Norm
;;      ;; Given this survey.
;;      survey <- (pure (distribution
;;                          [(list 'smoker 'tar 'nocancer)    323/800]
;;                          [(list 'smoker 'tar 'cancer)       57/800]
;;                          [(list 'nonsmoker 'tar 'nocancer)    1/800]
;;                          [(list 'nonsmoker 'tar 'cancer)     19/800]
;;                          [(list 'smoker 'notar 'nocancer)   18/800]
;;                          [(list 'smoker 'notar 'cancer)      2/800]
;;                          [(list 'nonsmoker 'notar 'nocancer) 38/800]
;;                          [(list 'nonsmoker 'notar 'cancer)  342/800]))

;;      ;; In a population where 80% smokes.
;;      population <- (distribution ['smoker 8/10] ['nonsmoker 2/10])

;;      ;; What would the incidence of cancer be?
;;      (list smoking tar cancer) <- (Identify (smoking tar cancer)
;;                 Intervening (smoking)
;;                 To (population)
;;                 WithModel (do genes <- ()
;;                               smoking <- (genes)
;;                               tar <- (smoking)
;;                               cancer <- (tar genes)
;;                               return (smoking tar cancer))
;;                 In survey)
;;      return cancer)
     

;; (define (causal-effect s)
;;   (Identify (cancer) Forcing (smoking) To (s)  
;;           (do genes <- ()
;;               smoking <- (genes)
;;               tar <- (smoking)
;;               cancer <- (tar genes)
;;               return (smoking tar cancer))
;;           survey))  

;(causal-effect 'smoker)
;(causal-effect 'nonsmoker)
