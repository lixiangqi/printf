#lang racket

;(define test%
;  (class object%
;    (define a 1)
;    (define (get-a) (+ 1 a))))

;(define (f x)
;  (/ x 1))
;(f 1)

(define (f x y)
  (+ (sqr x) (sqr y)))

(define (g x ret)
  (+ (sqr x) (sqr ret)))