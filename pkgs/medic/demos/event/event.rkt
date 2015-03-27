#lang racket

(require racket/gui)
(define mouse-over-object #f)

(define draw #f)

(define frame (new frame% [label "example"] [width 400] [height 400]))

(define test-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event evt)
      (case (send evt get-event-type)
        [(enter)
         (when mouse-over-object
           (set! draw #t))]
        [(left-down)
         (
         (printf "entered!\n")]))))

(new test-canvas% [parent frame])


(send frame show #t)



