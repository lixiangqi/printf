#lang racket

(define y 10)
(define z 2)
(define x -1)

(define (fact x y)
  (if (zero? x)
      1
      (* x 5)))

(fact 7 7)