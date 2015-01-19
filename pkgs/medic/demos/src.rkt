#lang racket

(define test%
  (class object%
    (field [a 1])
    (super-new)
    (define/public (get-a) a)))

(define (f a)
  (+ a 1))

(f 2)
(define t (new test%))
(send t get-a)