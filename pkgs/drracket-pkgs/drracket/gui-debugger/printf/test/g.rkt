#lang racket

(provide g)

(define (g x) 
  (printf "g: x=~a\n" x)
  (* x 3))

(g 2)