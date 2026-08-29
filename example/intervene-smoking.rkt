#lang racket

(require do/identify/identify)
(require do/monad)
(require do/monad/norm)


;; What would the rate of cancer be if we get our smoking rate to 1%?

(lDo Norm
     smokingHabits <- (distribution ['smoker 1/100] ['nonsmoker 99/100])
     
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
                  Setting (smoking) To (smokingHabits))

       return cancer)



;; EXAMPLE 2. Smoking causes cancer.
;; (define survey
;;   (distribution
;;      [(list 'smoker 'tar 'nocancer)    323/800]
;;      [(list 'smoker 'tar 'cancer)       57/800]
;;      [(list 'nonsmoker 'tar 'nocancer)    1/800]
;;      [(list 'nonsmoker 'tar 'cancer)     19/800]
;;      [(list 'smoker 'notar 'nocancer)   18/800]
;;      [(list 'smoker 'notar 'cancer)      2/800]
;;      [(list 'nonsmoker 'notar 'nocancer) 38/800]
;;      [(list 'nonsmoker 'notar 'cancer)  342/800]))

;; (lDo Norm
;;      habits <- (distribution ['smoker 2/10] ['nonsmoker 8/10])
;;      (list smoking tar cancer) <-
;;        (Intervene (distribution
;;                    [(list 'smoker 'tar 'nocancer)    323/800]
;;                    [(list 'smoker 'tar 'cancer)       57/800]
;;                    [(list 'nonsmoker 'tar 'nocancer)    1/800]
;;                    [(list 'nonsmoker 'tar 'cancer)     19/800]
;;                    [(list 'smoker 'notar 'nocancer)   18/800]
;;                    [(list 'smoker 'notar 'cancer)      2/800]
;;                    [(list 'nonsmoker 'notar 'nocancer) 38/800]
;;                    [(list 'nonsmoker 'notar 'cancer)  342/800])
;;                   WithModel (do genes <- ()
;;                                 smoking <- (genes)
;;                                 tar <- (smoking)
;;                                 cancer <- (tar genes)
;;                                 return (smoking tar cancer))
;;                   Setting (smoking) To (habits))
;;        return cancer)
