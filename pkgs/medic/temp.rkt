#lang racket

(require racket/gui
         framework)

; remember to diable snip insertion
(define frame (new frame% [label "test"] [width 800] [height 800]))

(define text (new text%))
(define snip-text (new text%))
(define s (new editor-snip% [editor snip-text]))
(send snip-text insert "a = 10 \nb = 12")

(define snip-text1 (new text%))
(define s1 (new editor-snip% [editor snip-text1]))
(send snip-text1 insert "a = 11 \nb = 13")

(define base-style
      (send (send snip-text get-style-list) find-named-style (editor:get-default-color-style-name)))

(new editor-canvas%
     [editor text]
     [parent frame])

(define sd (new style-delta%))
(send sd set-delta-background "Yellow")

(define sd1 (new style-delta%))
(send sd1 set-delta-foreground "DodgerBlue")
;(define d (send (send snip-text get-style-list) find-or-create-style base-style sd))
;(send s set-style d)

(send text insert "hello world\n")
(send text insert "hello world\n")
(send text insert "hello world\n")
(send text insert s)
(send text insert s1)
(send text insert "\nhello world")

(send text change-style sd1 0 10)
;(send snip-text change-style sd 0 (send snip-text last-position))
(send text change-style sd 0 50)
(send frame show #t)