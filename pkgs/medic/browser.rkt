#lang racket

(require racket/class
         racket/gui/base
         framework
         "graph-pasteboard.rkt")

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
    
    (define-values (graph-width graph-height) (send graph-panel get-size))
    (define graph-pb (new graph-pasteboard%
                          [width graph-width]
                          [height graph-height]))
 
    (new editor-canvas% 
         [parent log-panel]
         [style '(auto-hscroll)])
    (new canvas%
         [parent timeline-panel]
         [style '(hscroll)])
    (new editor-canvas%
         [parent graph-panel]
         [editor graph-pb])
      
    
         
         
      
    
    
    ))
    