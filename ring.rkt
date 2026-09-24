#lang racket

(struct ring (zero one plus mult invertible? div))

(provide (struct-out ring))
