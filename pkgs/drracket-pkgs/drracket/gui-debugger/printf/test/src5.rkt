#lang racket

;(define x 10)
;
;(define y 3)
;
;(+ 3 x)
;
;(set! x (add1 x))
;
;(define (g x)
;  (define y 3)
;  (define z 5)
;  (+ x 1))
;
;(define (f x)
;  (define y 3)
;  (+ x y))
;
;(g 5)
;
;(set! x (add1 x))

(define (g x)
  (define y 3)
  (define z 5)
  (+ x 1))

(g 5)