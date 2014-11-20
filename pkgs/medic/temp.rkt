#lang racket

(require racket/gui)

; remember to diable snip insertion
(define frame (new frame% [label "test"] [width 800] [height 800]))

(define text (new text%))
(define snip-text (new text%))
(define s (new editor-snip% [editor snip-text]))
(send snip-text insert "a = 10 \nb = 12")

(define snip-text1 (new text%))
(define s1 (new editor-snip% [editor snip-text1]))
(send snip-text1 insert "a = 11 \nb = 13")


(new editor-canvas%
     [editor text]
     [parent frame])

(send text insert s)
(send text insert s1)
(send frame show #t)