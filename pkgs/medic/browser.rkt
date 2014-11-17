#lang racket

(require racket/class
         racket/gui/base
         framework
         "graph-pasteboard.rkt"
         "timeline-canvas.rkt"
         "visual-util.rkt")

(provide make-trace-browser)

  
(define (make-trace-browser fn)
  (define frame (new trace-frame% [filename fn]))
  (send frame show #t))

(define trace-frame%
  (class (frame:basic-mixin frame%)
    (init-field (filename #f))
    (inherit get-area-container)
    (super-new (label (make-label))
               (width 1200)
               (height 800))
    (define widget (new widget% [parent (get-area-container)]))
    
    (define/private (make-label)
      (if filename
          (string-append filename " - Traces")
          "Traces"))))

(define widget%
  (class object%
    (init parent)
           
    (super-new)
    
    (define split-panel (new panel:vertical-dragable% [parent parent]))
    (define top-panel (new vertical-panel% [parent split-panel]))
    (define timeline-panel (new vertical-panel% [parent split-panel]))
    
    (define sub-split-panel (new panel:horizontal-dragable% [parent top-panel]))
    (define log-panel (new vertical-panel% [parent sub-split-panel]))
    (define graph-panel (new vertical-panel% [parent sub-split-panel]))
    
    (send split-panel begin-container-sequence)
    (send split-panel set-percentages (list 1/2 1/2))
    (send split-panel end-container-sequence)
    
    (send sub-split-panel begin-container-sequence)
    (send sub-split-panel set-percentages (list 1/2 1/2))
    (send sub-split-panel end-container-sequence)
    (new slider% [label "Timeline"] [min-value 0] [max-value 10] [parent timeline-panel])
    
    (define-values (graph-width graph-height) (send graph-panel get-size))
    (define graph-pb (new graph-pasteboard%
                          [raw-edges (get-raw-edges)]
                          [width graph-width]
                          [height graph-height]))
    
    #;(define graph-pb (new text%))
 
    (new editor-canvas% 
         [parent log-panel]
         [style '(auto-hscroll)])
    
    (define data (list 
                  (list "x" #f (list 3 7 5 9 10))
                  (list "y" #f (list -1 2 3 -4 5))
                  (list "z" #f (list -1 2.0 1/3 -4 5))
                  (list "x" #f (list #f #f #f #f #t))
                  (list "x > 5" #t (list #f #t #f #t #t))
                  (list "str" #f (list "hello" "world" 1 #t "time line" "core" "canvas"))))
    
    (define timeline-canvas (new timeline-canvas%
                                 ;[data (get-timeline-data)]
                                 [data data]
                                 [parent timeline-panel]
                                 [style '(hscroll vscroll)]))
    (send timeline-canvas init-auto-scrollbars 
          (send timeline-canvas get-actual-width) 
          (send timeline-canvas get-actual-height)
          0.0 0.0)
    (send timeline-canvas show-scrollbars #t #t)
    (new editor-canvas%
         [parent graph-panel]
         [editor graph-pb])
      
    
         
         
      
    
    
    ))
    