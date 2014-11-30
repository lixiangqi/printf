#lang racket

(require racket/gui/base
         (for-syntax images/icons/style pict)
         images/compile-time)

(provide aggregate-editor%)

(define aggregate-snip%
  (class editor-snip%
    (inherit get-extent)
    
    (super-new)
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define bh (box 0))
      (define rect-width 3)
      (send dc set-pen (send the-pen-list find-or-create-pen "LightGray" 0 'solid))
      (send dc set-brush (send the-brush-list find-or-create-brush "LightGray" 'solid))
      (get-extent dc x y #f bh)
      (send dc draw-rectangle x y rect-width (unbox bh))
      (super draw dc x y left top right bottom dx dy draw-caret))))

(define scrub-icon
  (compiled-bitmap 
   (bitmap-render-icon 
    (pict->bitmap (colorize (filled-ellipse 20 20) "tomato")) 0 glass-icon-material)))

(define scrub-snip%
  (class image-snip%
    (field [series null]
           [frame-width 400]
           [frame-height 400])
    
    (define/private (make-slider-bitmap)
      (define width 100)
      (define height 20)
      (define bm (make-object bitmap% width height #f #t))
      (define bm-dc (new bitmap-dc% [bitmap bm]))
      (send bm-dc set-pen (send the-pen-list find-or-create-pen "LightGray" 0 'solid))
      (send bm-dc set-brush (send the-brush-list find-or-create-brush "LightGray" 'solid))
      (send bm-dc draw-rounded-rectangle 0 0 width height)
      bm)
    
    (define scrub-slider%
      (class canvas%
        (inherit get-dc
                 refresh
                 get-parent)
        (super-new)
        
        (define slider-color "LightGray")
        (define thumb1-color "Orange")
        (define thumb2-color "Green")
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
        
        (define top-thumb 'thumb2)
        (define flag #f)
        
        (define/override (on-paint)
          (define dc (get-dc))
          (define frame-width (send (get-parent) get-width))
          (define rect-width (- frame-width (+ margin-space margin-space)))
          (send dc set-pen (send the-pen-list find-or-create-pen slider-color 0 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush slider-color 'solid))
          (send dc draw-rounded-rectangle margin-space margin-space rect-width rect-height)
          
          (send dc set-pen (send the-pen-list find-or-create-pen thumb1-color 0 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush thumb1-color 'solid))
          (send dc draw-ellipse thumb1-left-x thumb-top-y diameter diameter)
          
          (send dc set-pen (send the-pen-list find-or-create-pen thumb2-color 0 'solid))
          (send dc set-brush (send the-brush-list find-or-create-brush thumb2-color 'solid))
          (send dc draw-ellipse thumb2-left-x thumb-top-y diameter diameter)
          )
        
        (define/private (on-thumb1? x y)
          (and (>= x thumb1-left-x) (<= x thumb1-right-x)
               (>= y thumb-top-y) (<= y thumb-bottom-y)))
        
        (define/private (on-thumb2? x y)
          (and (>= x thumb2-left-x) (<= x thumb2-right-x)
               (>= y thumb-top-y) (<= y thumb-bottom-y)))
        
          ; override on-size
        (define/override (on-event event)
          (define mouse-x (send event get-x))
          (define mouse-y (send event get-y))
          (cond
            [(and (send event button-down? 'left)
                  (on-thumb2? mouse-x mouse-y)); use top-thumb
             (set! flag #t)]
            [(and flag (send event dragging?))
             (set! thumb2-left-x (- mouse-x radius))
             (set! thumb2-right-x (+ thumb2-left-x diameter))
             (refresh)]
            [(and flag (send event button-up? 'left))
             (set! flag #f)]
            
            ))))
    
    (define/public (initialize-series s) (set! series s))
    
    (define/override (adjust-cursor dc x y editorx editory event)
      (make-object cursor% 'arrow))
   
    (define/override (on-event dc x y editorx editory event)
      (when (send event button-down? 'left)
        (unless (send scrub-frame is-shown?)
          (send scrub-frame show #t)))
      (super on-event dc x y editorx editory event))
    
    (super-new)
    
    (define scrub-frame (new frame%
                             [label "Scrub View"]
                             [width frame-width]
                             [height frame-height]))
    (define main-panel (new vertical-panel% [parent scrub-frame]))
    (new editor-canvas% 
         [parent main-panel]
         [editor (new text%)])
    (new scrub-slider% [parent main-panel])
    
    
    
    ))

(define aggregate-editor%
  (class text%
    (init-field [data null])
    (inherit insert 
             begin-edit-sequence
             end-edit-sequence)
    
    (define/private (data-list->string l)
      (define items (map (lambda (p) (format "~a = ~v" (car p) (cdr p))) l))
      (apply string-append (add-between items "\n")))
    
    (define/private (display-aggregate-traces)
      (begin-edit-sequence)
      (for-each
       (lambda (l)
         (define scrub (make-object scrub-snip% scrub-icon))
         (send scrub initialize-series l)
         (send scrub set-flags (list 'handles-all-mouse-events))
         (insert scrub)
         (insert " ")
         (for-each (lambda (s)
                     (let* ([text (new text%)]
                            [snip (new aggregate-snip% [editor text])])
                       (send text insert (data-list->string s))
                       (send snip show-border #f)
                       (insert snip)))
                   l)
         (insert "\n\n"))
       data)
      (end-edit-sequence))
    
    (super-new)
    (display-aggregate-traces)))
    
    


