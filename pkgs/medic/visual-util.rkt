#lang racket

(require "graph-elements.rkt"
         (only-in racket/draw 
                  bitmap%
                  bitmap-dc%))

(provide add-edge
         get-graph-table
         create-node-bitmap)

(define snip-size 30)
(define nodes (make-hash))
(define edges (make-hash))

(define (add-edge from to edge-label from-label to-label)
  (when (and (object? from) (object? to))
    (unless (hash-has-key? nodes from) (hash-set! nodes from (new node%)))
    (unless (hash-has-key? nodes to) (hash-set! nodes to (new node%)))
    
    (define from-node (hash-ref nodes from))
    (define to-node (hash-ref nodes to))
    (unless (equal? from-node to-node)
      (let ([key (cons from to)])
        (unless (hash-has-key? edges key) 
          (hash-set! edges key (new edge% [from from-node] [to to-node] [label edge-label])))))))

(define (get-graph-table) (values nodes edges))

(define (create-node-bitmap)
  (define bm (make-object bitmap% snip-size snip-size))
  (define bm-dc (new bitmap-dc% [bitmap bm]))
  (send bm-dc set-brush "blue" 'solid)
  (send bm-dc draw-ellipse 0 0 snip-size snip-size)
  bm)

  