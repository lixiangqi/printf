#lang racket

(define x 2)
(define y 3)
(define r 5)
(define t 8)

(define (fact x)
  (if (zero? x)
      1
      (* x (fact (sub1 x)))))

(fact 3)