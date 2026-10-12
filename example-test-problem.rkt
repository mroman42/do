#lang racket

;; TEST PROBLEM.
;;
;; The single test problem is a basic problem in probabilistic inference.
;; Consider an illness with a certain known prevalence, for which we have a test
;; with a certain, known, specificity and sensibility. For a randomly selected
;; patient that tests negative, what is the posterior probability of illness?
;;
;; REFERENCES.
;;  - https://arxiv.org/pdf/1807.05609

(require "distributions.rkt")
(require "normDo.rkt")

(define prevalence
  (distribution ['(i) 1/3] ['(h) 2/3]))

(define (channel patient)
  (match patient
    ['i  (distribution ['(p) 3/4] ['(n) 1/4])]
    ['h  (distribution ['(p) 1/2] ['(n) 1/2])]))

(define (single-test-problem)
  (do
      (patient) <- prevalence
      (test) <- (channel patient)
      () <- (observe 'p test)
      return (patient)))

(define (single-test-problem-1)
  (do
      (patient) <- prevalence
      return (patient)))

(define (single-test-problem-2)
  (do
      (patient) <- prevalence
      (test) <- (channel patient) 
      return (patient test)))

(define (single-test-problem-3)
  (do
      (patient) <- prevalence
      (test) <- (channel patient)
      () <- (observe 'p test)
      return (patient test)))

(define (single-test-problem-r)
  (do
      (patient) <- prevalence
      (test) <- (channel patient)
      () <- (observe 'p test)
      return (patient)))

(printf "SINGLE TEST PROBLEM.~n~n")
(printf "Claim B.1 - Section 1.2.~n")
(printf "The solution of the single test problem is: ~a~n" (single-test-problem))
(printf "We present the solution, computed step-by-step:~n")
(printf "(i)   ~a~n" (single-test-problem-1))
(printf "(ii)  ~a~n" (single-test-problem-2))
(printf "(iii) ~a~n" (single-test-problem-3))
(printf "(r)   ~a~n" (single-test-problem-r))
(printf "~n~n")


;; UNCLEAR TEST PROBLEM.
;;
;; The unclear test problem is a modification of the test problem where we hold
;; some uncertainty about the result itself.
(define uncertainty
  (distribution ['(p) 3/4] ['(n) 1/4]))

(define (unclear-test-problem-pearl)
  (do 
      (result) <- uncertainty
      (patient) <- prevalence
      (test) <- (channel patient)
      () <- (observe test result)
      return (patient)))


(define (unclear-test-problem-jeffrey)
  (do 
      (result) <- uncertainty
      (patient) <- (do 
          (patient) <- prevalence
          (test) <- (channel patient)
          () <- (observe test result)
          return (patient))
      return (patient)))

(printf "UNCLEAR TEST PROBLEM.~n~n")
(printf "Claim B.2 - Figure 2.~n")
(printf "The unclear test problem has two solutions, depending on associativity.~n")
(printf "These are called Pearl's update and Jeffrey's update.~n")
(printf "Pearl's update computes as follows:   ~a~n" (unclear-test-problem-pearl))
(printf "Jeffrey's update computes as follows: ~a~n" (unclear-test-problem-jeffrey))
(printf "~n~n")


;; Step-by-step computations follow.
(define (unclear-test-problem-pearl-1)
  (do 
      (result) <- uncertainty
      return (result)))

(define (unclear-test-problem-pearl-2)
  (do 
      (result) <- uncertainty
      (patient) <- prevalence
      return (result patient)))


(define (unclear-test-problem-pearl-3)
  (do 
      (result) <- uncertainty
      (patient) <- prevalence
      (test) <- (channel patient)
      return (result patient test)))

(define (unclear-test-problem-pearl-4)
  (do 
      (result) <- uncertainty
      (patient) <- prevalence
      (test) <- (channel patient)
      () <- (observe test result)
      return (result patient test)))

(define (unclear-test-problem-pearl-r)
  (do 
      (result) <- uncertainty
      (patient) <- prevalence
      (test) <- (channel patient)
      () <- (observe test result)
      return (patient)))


(define (unclear-test-problem-pearl-u1)
  (do 
      (result) <- uncertainty
      return (result (do (patient) <- prevalence
                         return (patient)))))

(define (unclear-test-problem-pearl-u2)
  (do 
      (result) <- uncertainty
      (patient) <- prevalence
      return (result patient (do (test) <- (channel patient)
                                 return (test)))))

(define (unclear-test-problem-pearl-u3)
  (do 
      (result) <- uncertainty
      (patient) <- prevalence
      (test) <- (channel patient)
      return (result patient test (do () <- (observe test result)
                                      return ()))))
  

(displayln "Claim B.3 - Section 1.2.")
(displayln "Pearl's update, computed step-by-step.")
(printf "(i)   ~a~n" (unclear-test-problem-pearl-1))
(printf "(ii)  ~a~n" (unclear-test-problem-pearl-u1))
(printf "(n1)  ~a~n" (unclear-test-problem-pearl-2))
(printf "(iii) ~a~n" (unclear-test-problem-pearl-u2))
(printf "(n2)  ~a~n" (unclear-test-problem-pearl-3))
(printf "(iv)  ~a~n" (unclear-test-problem-pearl-u3))
(printf "(n3)  ~a~n" (unclear-test-problem-pearl-4))
(printf "(r)   ~a~n" (unclear-test-problem-pearl-r))
(displayln "")



;; Step-by-step: Jeffrey's update.
(define (unclear-test-problem-jeffrey-1)
  (do 
      (result) <- uncertainty
      return (result)))

(define (unclear-test-problem-jeffrey-2)
  (do 
      (result) <- uncertainty
      return (result (do (patient) <- prevalence
                          return (patient)))))

(define (unclear-test-problem-jeffrey-3)
  (do 
      (result) <- uncertainty
      return (result (do (patient) <- prevalence
                          (test) <- (channel patient)
                          return (patient test)))))

(define (unclear-test-problem-jeffrey-4)
  (do 
      (result) <- uncertainty
      return (result (do (patient) <- prevalence
                          (test) <- (channel patient)
                          () <- (observe test result)
                          return (patient test)))))

(define (unclear-test-problem-jeffrey-r1)
  (do 
      (result) <- uncertainty
      (patient) <- (do (patient) <- prevalence
                       (test) <- (channel patient)
                       () <- (observe test result)
                       return (patient))
      return (result patient)))

(define (unclear-test-problem-jeffrey-r2)
  (do 
      (result) <- uncertainty
      (patient) <- (do 
          (patient) <- prevalence
          (test) <- (channel patient)
          () <- (observe test result)
          return (patient))
      return (patient)))

(define (unclear-test-problem-jeffrey-u2)
  (do 
      (result) <- uncertainty
      return (result (do (patient) <- prevalence
                          return (patient (do (test) <- (channel patient)
                                              return (test)))))))


(displayln "Claim B.4 - Section 1.2.")
(displayln "Jeffreys's update, computed step-by-step:")
(printf "(i)   ~a~n"     (unclear-test-problem-jeffrey-1))
(printf "    (ii)  ~a~n" (unclear-test-problem-jeffrey-2))
(printf "    (iii) ~a~n" (unclear-test-problem-jeffrey-u2))
(printf "    (m2)  ~a~n" (unclear-test-problem-jeffrey-3))
(printf "    (iv)  ~a~n" (unclear-test-problem-jeffrey-4))
(printf "    (r1)  ~a~n" (unclear-test-problem-jeffrey-r1))
(printf "(r2)  ~a~n"     (unclear-test-problem-jeffrey-r2))
(printf "~n~n")
