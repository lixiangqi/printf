#lang racket

(define y 10)

(define (fact x y)
  (if (zero? x)
      1
      (* x 5)))

(fact 7 7)