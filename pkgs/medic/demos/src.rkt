#lang racket

(define test%
  (class object%
    (define a 1)
    (define (get-a) (+ 1 a))))

(define hi%
  (class object%
    (define a 1)
    (define b 2)
    (define (get-b) (+ 2 b))
    (define (get-a) (+ 1 a))))


;(let ([a (lambda (x) (+ 1 x))]
;      [b (+ 2 4)])
;  (+ (a 3) b))