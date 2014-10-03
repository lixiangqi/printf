#lang racket

(define x 10)

(define (g x)
  (define y 3)
  (define z 5)
  (+ x 1))

(define y 3)

(g 5)
