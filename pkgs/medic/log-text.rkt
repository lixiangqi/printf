#lang racket

(require racket/gui/base)

(provide log-text%)

(define log-text%
  (class text%
    (super-new)))