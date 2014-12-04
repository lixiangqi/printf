#lang racket

(provide log 
         aggregate
         edge
         timeline
         assert
         same?)

(define (log datum) (void))

(define (aggregate v . vs) (void))

(define (edge from-node to-node [edge-label #f] [from-label #f] [to-label #f] [arrow-color #f]) (void))

(define (timeline e) (void))

(define (assert cond) (void))

(define (same? e) (void))