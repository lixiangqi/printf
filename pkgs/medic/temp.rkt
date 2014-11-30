#lang racket

(require racket/gui
         framework
         (except-in pict frame text)
         images/icons/style)

; remember to diable snip insertion
(define frame (new frame% [label "test"] [width 400] [height 400]))

(define test-canvas%
  (class canvas%
    (inherit get-dc
             refresh)
    (super-new)
    (define dc (get-dc))
    (define m 10)
    (define n 10)
    (define/override (on-paint)
      (send dc draw-rounded-rectangle 30 30 200 20)
      (send dc draw-rectangle m n 10 40)
      )
    
    (define/override (on-event event)
      (when (send event dragging?)
        (printf "x=~v, y=~v\n" (send event get-x) (send event get-y))
        (set! m (send event get-x))
        (refresh)
        )
      
      )
    ))
(new test-canvas% [parent frame])
(send frame show #t)