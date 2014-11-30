#lang racket

(require racket/draw
         racket/gui/base)

(provide scrub-slider%)

(define scrub-slider%
      (class canvas%
        (init-field [step 1]
                    [frame-width 400])
        (inherit get-dc
                 refresh
                 get-parent)
        (super-new)
        
        (define slider-color "LightGray")
        (define thumb1-color "Orange")
        (define thumb2-color "Green")
        (define thumb1-light-color "Orange")
        (define thumb2-light-color "Green")
        (define thumb1-dark-color "Sienna")
        (define thumb2-dark-color "DarkGreen")
        
        (define margin-space 10)
        (define rect-height 6)
        (define diameter 16)
        (define radius (/ diameter 2))
        (define thumb-top-y (- margin-space (- radius (/ rect-height 2))))
        (define thumb-bottom-y (+ thumb-top-y diameter))
        (define thumb1-left-x margin-space)
        (define thumb1-right-x (+ thumb1-left-x diameter))
        (define thumb2-left-x margin-space)
        (define thumb2-right-x thumb1-right-x)
        (define label-y (+ thumb-bottom-y 5))
        
        (define slider-width frame-width)
        (define rect-width (- slider-width (+ margin-space margin-space diameter)))
        (define unit-width (/ rect-width step))
        
        (define last-thumb 'thumb2)
        (define thumb1-dragged? #f)
        (define thumb2-dragged? #f)
        (define thumb1-val 0)
        (define thumb2-val 0)
        (define dc (get-dc))
        
        (define/private (draw-thumb1)
          (send dc set-pen (send the-pen-list find-or-create-pen thumb1-color 0 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush thumb1-color 'solid))
          (send dc draw-ellipse thumb1-left-x thumb-top-y diameter diameter))
        
        (define/private (draw-thumb2)
          (send dc set-pen (send the-pen-list find-or-create-pen thumb2-color 0 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush thumb2-color 'solid))
          (send dc draw-ellipse thumb2-left-x thumb-top-y diameter diameter))
        
        (define/override (on-paint)
          (send dc set-pen (send the-pen-list find-or-create-pen slider-color 0 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush slider-color 'solid))
          (send dc draw-rounded-rectangle margin-space margin-space rect-width rect-height)
          (case last-thumb
            [(thumb1) (draw-thumb2)
                      (draw-thumb1)]
            [(thumb2) (draw-thumb1)
                      (draw-thumb2)])
          (send dc draw-text (format "~a" thumb1-val) (+ thumb1-left-x radius) label-y)
          (send dc draw-text (format "~a" thumb2-val) (+ thumb2-left-x radius) label-y))
        
        (define/private (on-thumb1? x y)
          (and (>= x thumb1-left-x) (<= x thumb1-right-x)
               (>= y thumb-top-y) (<= y thumb-bottom-y)))
        
        (define/private (on-thumb2? x y)
          (and (>= x thumb2-left-x) (<= x thumb2-right-x)
               (>= y thumb-top-y) (<= y thumb-bottom-y)))
        
        (define/private (on-thumb? thumb x y)
          (case thumb
            [(thumb1) (on-thumb1? x y)]
            [(thumb2) (on-thumb2? x y)]))
        
        (define/private (set-thumb-dragged thumb b)
          (case thumb
            [(thumb1)
             (if b
                 (set! thumb1-color thumb1-dark-color)
                 (set! thumb1-color thumb1-light-color))
             (set! thumb1-dragged? b)]
            [(thumb2)
             (if b
                 (set! thumb2-color thumb2-dark-color)
                 (set! thumb2-color thumb2-light-color))
             (set! thumb2-dragged? b)]))
        
        (define/private (overlay?)
          (and (= thumb1-left-x thumb2-left-x)
               (= thumb2-right-x thumb2-right-x)))
        
        (define/private (get-current-val mouse-x)
          (define val (inexact->exact (round (/ (- mouse-x margin-space) unit-width))))
          (when (< val 0) (set! val 0))
          (when (> val step) (set! val step))
          val)
        
        (define/override (on-event event)
          (define mouse-x (send event get-x))
          (define mouse-y (send event get-y))
          (cond
            [(send event button-down? 'left)
             (if (overlay?)
                 (when (on-thumb? last-thumb mouse-x mouse-y)
                   (set-thumb-dragged last-thumb #t))
                 (if (on-thumb1? mouse-x mouse-y)
                     (set-thumb-dragged 'thumb1 #t)
                     (when (on-thumb2? mouse-x mouse-y)
                       (set-thumb-dragged 'thumb2 #t))))]
            
            [(send event dragging?)
             (cond
               [thumb1-dragged?
                (set! thumb1-val (get-current-val mouse-x))
                (set! thumb1-left-x (+ (* thumb1-val unit-width) margin-space))
                (set! thumb1-right-x (+ thumb1-left-x diameter))
                (set! last-thumb 'thumb1)
                (refresh)]
               [thumb2-dragged? 
                (set! thumb2-val (get-current-val mouse-x))
                (set! thumb2-left-x (+ (* thumb2-val unit-width) margin-space))
                (set! thumb2-right-x (+ thumb2-left-x diameter))
                (set! last-thumb 'thumb2)
                (refresh)])]
            
            [(send event button-up? 'left)
             (cond
               [thumb1-dragged? 
                (set-thumb-dragged 'thumb1 #f)
                (refresh)]
               [thumb2-dragged? 
                (set-thumb-dragged 'thumb2 #f)
                (refresh)])]))))