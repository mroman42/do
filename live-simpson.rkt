#lang racket

(require "notation/normDo.rkt")
(require "intervene/algorithm-id.rkt")
(require "intervene/intervene.rkt")

(define/table data
  (['(X1 A remission)  28]
   ['(X1 A failure)     2]
   ['(X1 B remission)  78]
   ['(X1 B failure)    12]
   ['(X2 A remission)  33]
   ['(X2 A failure)    12]
   ['(X2 B remission)  10]
   ['(X2 B failure)     5]))

