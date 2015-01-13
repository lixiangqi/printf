#lang racket

;(define test%
;  (class object%
;    (define a 1)
;    (define (get-a) (+ 1 a))))

;(define (f x)
;  (/ x 1))
;(f 1)

(define (inc-counter) (void))
(define (inc x) (+ x 1))

(define (g)
  (define x (inc 4))
  (inc-counter)
  (+ x 1))

(define (f)
  (define x (inc 4))
  (inc-counter)
  (+ x 2))

(g)
(f)