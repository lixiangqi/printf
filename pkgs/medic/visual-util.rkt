#lang racket

(provide add-edge
         get-raw-edges)

(define snip-size 30)
(define raw-edges (make-hash))

(define (add-edge from to edge-label from-label to-label)
  (when (and (object? from) (object? to))
    (hash-set! raw-edges (cons from to) (list edge-label from-label to-label))))

(define (get-raw-edges) raw-edges)



  