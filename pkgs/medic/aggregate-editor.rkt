#lang racket

(require racket/gui/base)

(define aggregate-snip%
  (class editor-snip%
    (inherit get-extent)
    
    (super-new)
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define bh (box 0))
      (define rect-width 5)
      (send dc set-pen (send the-pen-list find-or-create-pen "gray" 0 'solid))
      (send dc set-brush (send the-brush-list find-or-create-brush "gray" 'solid))
      (get-extent dc x y #f bh)
      (send dc draw-rectangle x y rect-width bh)
      (super draw dc x y left top right bottom dx dy draw-caret))))

(define scrub-icon%
  (class 
    