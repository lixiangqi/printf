#lang racket

(require mrlib/graph
         racket/gui/base)

(provide graph-pasteboard%
         graph-image-snip%)

(define graph-pasteboard%
  (class (graph-pasteboard-mixin pasteboard%)
    (super-new)))

(define graph-image-snip%
  (class (graph-snip-mixin image-snip%)
    (super-new)
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-text "label" (+ x 30) (+ y 30))
      (super draw dc x y left top right bottom dx dy draw-caret))))

