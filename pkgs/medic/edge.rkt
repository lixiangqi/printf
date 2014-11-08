#lang racket

(require "graph-elements.rkt")

(provide edge)

(define nodes (make-hash))
(define edges (make-hash))

(define (edge from-node to-node [edge-label #f] [from-label #f] [to-label #f])
  (void))



#;(define (edge from-node to-node 
              #:edge-label [edge-label ""] 
              #:from-label [from-label ""] 
              #:to-label [to-label ""])
  (let ([new-from-label (if (equal? from-label "") (format "~a" from-node) from-label)]
        [new-to-label (if (equal? to-label "") (format "~a" to-node) to-label)])
    (when (and (object? from-node) (object? to-node))
      (unless (hash-has-key? nodes from-node) (hash-set! nodes from-node (new node%)))
      (unless (hash-has-key? nodes to-node) (hash-set! nodes to-node (new node%)))
      (unless (equal? from-node to-node)
        (let ([key (cons from-node to-node)])
          (unless (hash-has-key? edges key) 
            (hash-set! edges key (new edge% [from from-node] [to to-node] [label edge-label]))))))))


; record all info (arguments of edge) and then call some function in another space to store these info and generate nodes and edges