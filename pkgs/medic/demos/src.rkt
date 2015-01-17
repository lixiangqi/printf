#lang racket

;(define test%
;  (class object%
;    (define a 1)
;    (define (get-a) (+ 1 a))))
;
;(define hi%
;  (class object%
;    (define a 1)
;    (define b 2)
;    (define (get-b) (+ 2 b))
;    (define (get-a) (+ 1 a))))

(define (f x) (+ x 1))

(f (/ 1 0))
