#lang racket

(define (fact x)
  (if (zero? x)
      1
      (* x 5)))
(fact 3)

(define (fg x)
  (+ x 14)
  (+ x 20)
  (+ x 1)
  (+ x 7)
  )

(fg 5)

