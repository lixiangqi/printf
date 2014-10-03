#lang racket

(require "f.rkt")
(define (fact x)
  (if (zero? x)
      1
      (* x (fact (sub1 x)))))

(fact 3)
(f 4)

