#lang racket

(provide log 
         edge
         timeline
         assert)

(define (log datum) (void))

(define (edge from-node to-node [edge-label #f] [from-label #f] [to-label #f]) (void))

(define (timeline id) (void))

(define (assert cond) (void))