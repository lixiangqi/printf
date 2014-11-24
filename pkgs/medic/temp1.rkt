#lang racket

(require racket/gui
         framework)
(define frame (new frame% [label "test"] [width 800] [height 800]))

;(define text (new text%))
;(new editor-canvas%
;     [editor text]
;     [parent frame])

(define list-box (new list-box%
                      (label "List Box")
                      (parent frame)
                      (choices (list "Item 0"
                                     "Item 1"
                                     "Item 2"))
                      (style (list 'multiple
                                   'column-headers))
                      (columns (list "First Column"))))
(send frame show #t)
