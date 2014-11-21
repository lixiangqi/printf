#lang racket

(define y 10)

(define (fact x)
  (if (zero? x)
      1
      (* x 5)))
