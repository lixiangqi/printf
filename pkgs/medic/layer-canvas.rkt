#lang racket

(provide layer-canvas%)

(define layer-canvas%
  (class canvas%
    (super-new)