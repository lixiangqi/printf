#lang racket

(require racket/gui
         framework
         (except-in pict frame text)
         images/icons/style)

; remember to diable snip insertion
(define frame (new frame% [label "test"] [width 800] [height 800]))

(define bm (pict->bitmap (colorize (filled-ellipse 20 20) "tomato")))
(define icon (bitmap-render-icon bm 0 glass-icon-material))
(define image-snip (make-object image-snip% icon))

(define line-bm (pict->bitmap (colorize (filled-rectangle 5 27) "gray")))
(define image-snip1 (make-object image-snip% line-bm))

(define test-snip%
  (class editor-snip%
    (super-new)
    (define/override (on-event dc x y ex ey event)
      (when (send event button-down? 'left)
        (printf "button down!!\n"))
      (super on-event dc x y ex ey event))
    
    (define/override (adjust-cursor dc x y ex ey event)
      (make-object cursor% 'arrow))
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define bw (box 0))
      (define bh (box 0))
      (send dc set-pen "gray" 0 'solid)
      (send dc set-brush "gray" 'solid)
      (send this get-extent dc x y bw bh)
      (printf "bw=~v, bh=~v\n" bw bh)
      (send dc draw-rectangle x y 5 42)
      (printf "entered\n")
      (super draw dc x y left top right bottom dx dy draw-caret))))

(define text (new text%))
(define snip-text (new text%))
(define s (new test-snip% [editor snip-text]))


;(define s (new editor-snip% [editor snip-text]))
(send snip-text insert "a = 10 ")
;(send snip-text insert "a = 10 \nb = 12")

(define snip-text1 (new text%))
(define s1 (new editor-snip% [editor snip-text1]))
(send snip-text1 insert "a = 11 \nb = 13")

(send s show-border #f)
(send s1 show-border #f)
(define l (box 0))
(define t (box 0))
(define r (box 0))
(define b (box 0))
(send s get-margin l t r b)
(printf "l=~v, t=~v, r=~v, b=~v\n" l t r b)
(define snip-text2 (new text%))
(define s2 (new editor-snip% [editor snip-text2]))
(send snip-text2 insert "a = 11 \nb = 13")

(define snip-text3 (new text%))
(define s3 (new editor-snip% [editor snip-text3]))
(send snip-text3 insert "a = 11 \nb = 13")

(send s2 show-border #f)
(send s3 show-border #f)

(new editor-canvas%
     [editor text]
     [parent frame])

(send text insert "hello world\n")
(send text insert "hello world\n")
(send text insert "hello world\n")
(send text insert image-snip)
(send text insert " ")
(send text insert s)
;(send text insert s1)
(send text insert "\n")
(send text insert s2)
(send text insert s3)

(send text insert "\nhello world")

(define w (box 0))
(define h (box 0))
(send snip-text get-extent w h)
(printf "w=~v, h=~v\n" w h)

(send frame show #t)