#lang racket

(require "graph-elements.rkt")

(provide add-edge
         get-graph-table)

(define nodes (make-hash))
(define edges (make-hash))

(define (add-edge from-node to-node edge-label from-label to-label)
  (when (and (struct? from-node) (struct? to-node))
      (unless (hash-has-key? nodes from-node) (hash-set! nodes from-node (new node%)))
      (unless (hash-has-key? nodes to-node) (hash-set! nodes to-node (new node%)))
      (unless (equal? from-node to-node)
        (let ([key (cons from-node to-node)])
          (unless (hash-has-key? edges key) 
            (hash-set! edges key (new edge% [from from-node] [to to-node] [label edge-label])))))))

(define (get-graph-table) (values nodes edges))

  