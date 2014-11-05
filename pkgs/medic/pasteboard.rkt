#lang racket

(require mrlib/graph
         racket/gui
         (only-in 2htdp/image circle bitmap))

(define f (new frame% [label "graph"]
               [width 200]
               [height 200]))



(define graph-pasteboard%
  (class (graph-pasteboard-mixin pasteboard%)
    (super-new)))

(define p (new graph-pasteboard%))

(define ec (new editor-canvas% 
                [parent f]
                [editor p]))

; disable resize feature
(define graph-image-snip%
  (class (graph-snip-mixin image-snip%)
    
    (super-new)
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-text "label" 80 50)
      (super draw dc x y left top right bottom dx dy draw-caret))))


(define bm (make-object bitmap% 30 30))
(define bm-dc (new bitmap-dc% [bitmap bm]))
(send bm-dc set-brush "blue" 'solid)
(send bm-dc draw-ellipse 0 0 30 30)

(define g (make-object graph-image-snip% bm))


(send p insert g 50 50)

(send f show #t)

    