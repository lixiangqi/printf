#lang racket

(provide log 
         aggregate
         edge
         timeline
         assert
         same?)

(define (log datum) (void))

(define (aggregate v . vs) (void))

(define (edge from-node to-node [edge-label ""] [from-label ""] [to-label ""] [color #f]) (void))

(define (timeline e) (void))

(define (assert cond) (void))

(define (same? e) (void))