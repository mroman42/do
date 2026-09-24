#lang racket

(struct ring (zero one plus mult ninv? div))

(provide (struct-out ring))
