#lang racket

(require "g.rkt")
(provide f)

(define (f x)
  (printf "f: x=~a\n" x)
  (+ x (g x)))

(f 2)