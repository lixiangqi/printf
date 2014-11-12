#lang racket

(require racket/gui/base)

(provide timeline-canvas%)

(define timeline-canvas%
  (class canvas%
    (inherit get-dc)
    (super-new)
    
    (define data (list (cons "x" (list 3 7 5 9 10))
                       (cons "x > 5" (list #f #t #f #t #t))
                       (cons "str" (list "hello" "world"))))
    
    (define timeline-space 50)
    (define start 50) ; depends on the data label width
    ; ForestGreen to be true
    ; Firebrick to be false
    (define square-size 50)
    (define dc (get-dc))
    
    ;(define/private 
    (define/override (on-paint)
      
      
      (send dc set-brush "Firebrick" 'solid)
      (send dc set-pen "White" 0 'solid)
      (send dc draw-rectangle start start square-size square-size)
      (send dc set-brush "ForestGreen" 'solid)
      (send dc draw-rectangle (+ square-size start) start square-size square-size)
      
      (define bm (make-object bitmap% (- square-size 2) 25 #f #t))
      (define bm-dc (new bitmap-dc% [bitmap bm]))
      (send bm-dc draw-text "hellohellohello" 0 0)
      (send dc draw-bitmap bm start start)
      (send dc draw-text "..." 70 80)
      
      #;(let loop ([i 0])
        (when (< i 10)
          (send dc draw-rectangle (+ start (* square-size i)) start square-size square-size)
          (loop (add1 i))))
      ;(send dc draw-line 0 50 4000 50)
      #;(send dc draw-rectangle 3948 60 50 50))
      
      
    
    ))