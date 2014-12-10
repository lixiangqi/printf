#lang racket

;(define test%
;  (class object%
;    (define a 1)
;    (define/public (get-a) a)
;    (super-new)))
;
;(define t (new test%))
;(send t get-a)

(define (f x)
  (+ x 1))

(f 2)

