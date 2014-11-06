#lang racket

(require mrlib/graph
         racket/gui
         (only-in 2htdp/image circle bitmap))

(define f (new frame% [label "graph"]
               [width 500]
               [height 500]))



(define graph-pasteboard%
  (class (graph-pasteboard-mixin pasteboard%)
    (super-new)
    
;    (define/augment (on-move-to snip x y dragging?)
;      (begin-edit-sequence)
;      (send snip draw
      
    ))

(define p (new graph-pasteboard%))

(define ec (new editor-canvas% 
                [parent f]
                [editor p]))

; disable resize feature
(define graph-image-snip%
  (class (graph-snip-mixin image-snip%)
    
    (super-new)
    
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-text "label" (+ x 30) (+ y 30))
      (super draw dc x y left top right bottom dx dy draw-caret)
      )))

(define (create-node-bitmap)
  (define bm (make-object bitmap% 30 30))
  (define bm-dc (new bitmap-dc% [bitmap bm]))
  (send bm-dc set-brush "blue" 'solid)
  (send bm-dc draw-ellipse 0 0 30 30)
  bm)

(define dark-color "DarkGray")
(define light-color "Gray")

(define g (make-object graph-image-snip% (create-node-bitmap)))
(define g1 (make-object graph-image-snip% (create-node-bitmap)))
(add-links/text-colors g g1
           (send the-pen-list find-or-create-pen dark-color 0 'solid)
           (send the-pen-list find-or-create-pen light-color 0 'solid)
           (send the-brush-list find-or-create-brush dark-color 'solid)
           (send the-brush-list find-or-create-brush light-color 'solid)
           (make-object color% dark-color)
           (make-object color% light-color)
           0 0
           "edge")


(send p insert g 50 50)
(send p insert g1 80 80)

(send f show #t)

    