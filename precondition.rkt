#lang racket

(require racket/format)

(define-syntax-rule (precondition predicate msg ...)
  (unless predicate (error msg ...)))

(provide precondition)
